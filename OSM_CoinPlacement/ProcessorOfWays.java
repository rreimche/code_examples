package core;

import org.json.simple.JSONArray;

import java.util.*;
import java.util.function.DoubleFunction;
import java.util.function.Function;

/*
 * Used to process Ways in two... well, ways:
 * 1. To filter a number of ways so that only those are left that are reachable from one another (a connected graph).
 * 2. To create coins so that they are placed on the Ways (intersect with them) with a given density.
 *
 * For some methods there are two implementation of various parts – one with loops and another one with the Stream API.
 * The ones with loops are chosen because it seems that they provide faster calculation:
 * https://jaxenter.com/java-performance-tutorial-how-fast-are-the-java-8-streams-118830.html
 *
 * In the case of AWS Lambda faster is cheaper, which was a critical argument for this decision.
 *
 * @author rreimche
 */
public class ProcessorOfWays {

    // The following fields are used by filterWays(). They are declared for the class instance
    // to make the inner workings of filterWays() more readable.
    boolean[][] examined;
    Way[] ways;
    boolean[] wayIsReachable;
    Queue<Integer> baseWaysQueue;

    /*
     * Creates a number of coins to place on each way trying to preserve some space between them. This method
     * does not guarantee that the desired density will be provided any two of the placed coins.
     *
     * @param density the desired number of meters between coins (for example, 3 means 1 cookie every 3 meters)
     * @param ways Ways to create coins for
     *
     * @return Coordinates of the created coins
     */
    public List<Coordinate> createCoins(int density, List<JSONArray> ways) {
        // Either createCoinsRelaxed(density, ways or createCoinsStrict(density, ways) should be called here.
        return createCoinsRelaxed(density, ways);
    }

    /*
     * DEPRECATED IN FAVOR OF {@link #createCoinsRelaxed(int,List<JSONArray>) createCoinsRelaxed}
     *
     * Creates a number of coins to place on each way trying preserving some space between them (density range).
     *
     * This method places coins so that no two coins violate the density constraint except for the coins that
     * are placed inside the density range of a ways intersection.
     *
     * It is allowed for the coins to violate the density range at ways intersections, because maintaining
     * the density on intersections is very complex and pressumably time expensive for implementeation
     * and at the same time it delivers almost no value for the application.
     *
     * Another benefit of this decision is that, provided that computation expenses mean money expenditure
     * in the case of AWS Lambdas, it provides a somehow more economical solution without any noticable
     * loss of the user experience or game meaning.
     *
     * CONTEXT AND IDEA FOR SOLUTION
     *
     * At this point we should have one or more ways. In the case of several ways
     * they all should be intersecting on one node (see filterStreets()).
     * At first we get all the nodes into one array of pairs of (coord: Coordinate, ifNodeHasCookie: bool).
     * Then for every node of this array (current node) we go through all the other nodes.
     * For every of the other nodes we check if it is in the density range and, if so, –
     * if it has a cookie already. For all the other nodes that are inside the density range
     * and have no cookie we do nothing, but if there is at least one other node inside
     * density range with a cookie – we break the current cycle and start again with the next
     * current node. If no other nodes for the current node has a cookie, place a cookie
     * at the current node.
     *
     * @param density the desired number of meters between coins (for example, 3 means 1 cookie every 3 meters)
     * @param ways Ways to create coins for
     *
     * @return Coordinates of the created coins
     */
    private List<Coordinate> createCoinsStrict(int density, List<JSONArray> ways) {

        /*


        ArrayList<Paar<Coordinate, Boolean>> allNodesAndCookies = new ArrayList();

        // Get all nodes into one array
        try {
            for (JSONArray way : ways) {
                for (Object o : way) {
                    Coordinate coord = new Coordinate((JSONArray) o);
                    allNodesAndCookies.add(new Paar(coord, false));
                }
            }
        }catch (Exception e){
            System.out.println(e);
        }

        /*ways.stream()
                .forEach(nodes -> {
                    nodes.stream()
                            .forEach(node -> {
                                Coordinate coord = new Coordinate((JSONArray) node);
                                allNodesAndCookies.add(new Paar(coord, false));
                            });
                });*/

        // Place Cookies

        for( Paar<Coordinate, Boolean> currentNodeAndCookie : allNodesAndCookies){

            // Check if there is a cookie inside the density range

            boolean noCookiePlease = false;

            for( Paar<Coordinate, Boolean> otherNodeAndCookie : allNodesAndCookies){
                if( distance(currentNodeAndCookie.getKey(), otherNodeAndCookie.getKey()) <= (double)density
                        && otherNodeAndCookie.getValue() ){
                    noCookiePlease = true;
                    break;
                }
            }

            if(noCookiePlease){
                continue;
            }



            // Place a cookie.

            currentNodeAndCookie.setValue(true);
        }

        // Generate result
        ArrayList<Coordinate> result = new ArrayList();

        for( Paar<Coordinate, Boolean> nodeAndCookie : allNodesAndCookies){
            // if has cookie, add to end result
            if(nodeAndCookie.getValue()) {
                result.add(nodeAndCookie.getKey());
            }
        }

        return result;
    }

    /*
     * Creates a number of coins to place on each way preserving some space between them (density range).
     *
     * This method placed coins so that two coins lying on two different ways may violate the density constraint.
     *
     * This way a lot of computation time is saved compared to the
     * {@link #createCoinsStrict(int, List<JSONArray>) createCoinsStrict} method. This happens because
     * there is no need to check the position of a node to be placed against all already placed nodes. This
     * provides a supposedely serious economical benefits, because computation time is money expenditure on AWS Lambda.
     *
     * This method walks through every way only once.
     *
     * POSSIBLE OPTIMIZATION: Allow duplicate coins. Then we can save on every result.contains().
     * (Questionable) POSSIBLE OPTIMIZATION: Use a search tree instead of an ArrayList for result to provide faster
     * result.contains(). It should be examined if we will win or loose this way because adding coins will cost more...
     * OPTIMIZATION POSSIBILITY: Use a sorted array.
     *
     *
     * @param density the desired number of meters between coins (for example, 3 means 1 cookie every 3 meters)
     * @param ways Ways to create coins for
     *
     * @return Coordinates of the created coins
     */
    private List<Coordinate> createCoinsRelaxed(int density, List<JSONArray> ways) {
        ArrayList<Coordinate> result = new ArrayList();
        try {

            // For every way
            for (JSONArray way : ways) {

                // If there are no nodes in the way, proceed to the next way.
                if (way.size() == 0) continue;

                // Place a coin on the first node.
                Coordinate firstNode = new Coordinate((JSONArray) way.get(0));
                if( !result.contains(firstNode) ) result.add(firstNode);

                // If there is only one node in this way, we're done with it. Proceed to the next way.
                if (way.size() == 1) continue;

                // Go through all the nodes of the current way except the 0th which was processed earlier.
                for (int i = 1; i < way.size() - 1; i++) {

                    // Use two consecutive nodes of the way
                    Coordinate thisNode = new Coordinate((JSONArray) way.get(i));
                    Coordinate nextNode = new Coordinate((JSONArray) way.get(i + 1));

                    // If the distance between the two consecutive nodes fulfills the density constraint,
                    // add a coin on the next node.
                    if ( ProcessorOfWays.distance(thisNode, nextNode) >= density ){
                        if( !result.contains(nextNode)) result.add(nextNode);
                    }
                }
            }

        } catch (Exception e){
            System.out.println(e);
            return null;
        }

        return result;

    }

    /*
     * TEMPORALLY ABANDONED: It is decided for now that placing coins on nodes is sufficient and therefore we
     * are not trying to place coins between the node to maintain the desired coin density in the case when
     * two consecutive nodes are too far away from each other. TODO In future this method should be finished
     * to provide more accurate coin placement.
     *
     * Returns coins for a part of the way between two nodes if the nodes are too far away from each other.
     *
     * "Too far away" is more than {@code density}.
     *
     * Creates coins so that they are placed on an imaginary direct line between the two nodes.
     *
     * Consider two nodes of a way that are 3 densities distant from each other. In this case we should have
     * 4 coins placed: 1 for each node and another 2 between them. This function helps to place these two that are
     * placed between them. Without it the only the coins on the nodes are created.
     *
     * Possibly useful links:
     * https://stackoverflow.com/questions/30787999/find-a-longitude-given-a-pair-of-lat-long-and-an-offset-latitude
     * https://www.google.com/search?client=safari&rls=en&q=find+a+poin+by+offset+in+WGS84&ie=UTF-8&oe=UTF-8
     * https://www.google.com/search?client=safari&rls=en&q=wgs84+meter+to+degree&ie=UTF-8&oe=UTF-8
     * https://math.stackexchange.com/questions/2045174/how-to-find-a-point-between-two-points-with-given-distance
     *
     * @parameter density The desired density of coin placement.
     * @parameter either Either node.
     * @parameter other Other node.
     *
     * @return List<Coordinate> which represent the created coins.
     */
    private List<Coordinate> createCoinsForDistantNodes(int density, Coordinate either, Coordinate other){
        /*
            x3 = x1 + d/D(x2-x1)
            y3 = y1 + d/D(y2-y1)

            d – density
            D – distance
         */

        /*double distance = ProcessorOfWays.distance(either,other);
        double distanceDone = 0;

        while(distanceDone < distance){
            //double newLat = either.getLatitude() + density
        }*/


        return null;
    }

    /*
     * Filters out Ways that neither have intersections with the Way that is the closest
     * to the player spawn position nor with any of the ways that intersect with it.
     *
     * @param input a number of Ways
     * @param playerSpawn player spawn point
     *
     * @return a list of ways connected with each other (forming a connected graph)
     */
    public List<JSONArray> filterWays(List<JSONArray> input, Coordinate playerSpawn){

        // We'll need to convert JSONArrays to Ways for the better.
        this.ways = new Way[input.size()];

        for(int i = 0; i < input.size() ; i++){

            JSONArray way = input.get(i);

            Coordinate[] coords = new Coordinate[way.size()];

            for(int j = 0; j < way.size() ; j++){
                JSONArray node = (JSONArray) way.get(j);
                coords[j] = new Coordinate((double) node.get(1), (double) node.get(0));
            }

            this.ways[i] = new Way(i, coords);
        }


        // Find the node that is the nearest to the playerSpawn (euclidian distance).
        //      - create an array with (nodeId, distanceFromSpawn)
        //      - get the nodeId with min distance

        Paar<Coordinate, Double> minDistanceNode = null;

        // If we have several nodes with the same distance from playerSpawn then we
        // take the first one.
        // TODO: In future we could select the one that belongs to the largest way. Examine if it will provide advantage.

        for(Way way : this.ways){
            for( int i = 0; i < way.length(); i++ ) {
                Coordinate nodeCoords = way.getNodeAtPosition(i);
                Double distance = distance(playerSpawn, nodeCoords);

                if( minDistanceNode == null ){

                    minDistanceNode = new Paar<>(nodeCoords, new Double(distance));

                } else if( distance.doubleValue() < minDistanceNode.getValue().doubleValue() ) {

                    minDistanceNode = new Paar<>(nodeCoords, new Double(distance));

                }
            }
        }

        // Get a way that has this node. The algorithm stops when one way passing through the node is found,
        //      because all other ways that intersect with this one will be found in the part IV.


        // wayIsReachable[i] == true <=> The way i is reachable from the way that is the nearest to the playerSpawn
        this.wayIsReachable = new boolean[this.ways.length];

        boolean found = false;
        this.baseWaysQueue = new LinkedList<>();

        for(Way way : this.ways){
            for (int i = 0; i < way.length(); i++) {
                Coordinate nodeCoords = way.getNodeAtPosition(i);

                if(minDistanceNode.getKey().equals(nodeCoords)){
                    this.wayIsReachable[way.getId()] = true;
                    //baseWayId = way.getId();
                    baseWaysQueue.add(way.getId());
                    found = true;
                    break;
                }
            }

            // If a way going through the node is found, no need to continue.
            if (found) break;
        }

        // Find all ways that are reachable from the first one
        // (that are connected with it through intersections)

        this.examined = new boolean[this.ways.length][this.ways.length];

        while(!baseWaysQueue.isEmpty()){
            Integer baseWayId = baseWaysQueue.remove();

            // The first way Fill possibleIntersectionCoordinates for the first way

            CoordsToPossibleIntersectionMapping possibleIntersectionCoordinates = initForBaseWay(this.ways[baseWayId]);

            // Find intersections of the  way with all the others if such are present,
            // mark correspondent ways as reachable.

            findIntersections(baseWayId, possibleIntersectionCoordinates);

            // Proceed to the next Way

        }

        // Return only ways that together are a connected graph closest to the playerSpawn.
        List<JSONArray> reachableWays = new ArrayList<>();
        for(int i = 0; i < this.wayIsReachable.length; i++){
            if ( this.wayIsReachable[i] ) {
                reachableWays.add(this.ways[i].toJSONArrayWithDoubles());
            }
        }

        return reachableWays;
    }

    /*
     * Computes distance between two Coordinates in meters
     * Uses Haversine formula, adapted from https://www.movable-type.co.uk/scripts/latlong.html
     * and from https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula
     *
     */
    public static Double distance(Coordinate either, Coordinate other){
        // this commented implementation seems to be erroneuos
        DoubleFunction<Double> deg2rad = deg -> new Double(deg * (Math.PI/180));

        double R = 6371; // Radius of the earth in km
        double dLat = deg2rad.apply(other.getLatitude() - either.getLatitude());
        double dLon = deg2rad.apply(other.getLongitude() - either.getLongitude());
        double a =
                Math.sin(dLat/2) * Math.sin(dLat/2) +
                        Math.cos(deg2rad.apply(either.getLatitude())) * Math.cos(deg2rad.apply(other.getLatitude())) *
                                Math.sin(dLon/2) * Math.sin(dLon/2)
                ;
        double c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
        double d = R * c; // Distance in km
        return new Double(d * 1000); // Distance in meters

    }

    private CoordsToPossibleIntersectionMapping initForBaseWay(Way baseWay){
        CoordsToPossibleIntersectionMapping possibleIntersectionCoordinates = new CoordsToPossibleIntersectionMapping();

        for (int i = 0; i < baseWay.length(); i++) {

            Coordinate nodeCoords = baseWay.getNodeAtPosition(i);

            possibleIntersectionCoordinates.add(nodeCoords);
        }

        return possibleIntersectionCoordinates;
    }

    private void findIntersections(int baseWayId, CoordsToPossibleIntersectionMapping possibleIntersectionCoordinates) {
        // To note here is that .filer.forEach should work here so that the chain
        // functions on element basis, not on Collection basis
        // See https://winterbe.com/posts/2014/07/31/java8-stream-tutorial-examples/
        Arrays.stream(this.ways)
                .filter(way -> way.getId() != baseWayId
                        && !this.examined[baseWayId][way.getId()]
                        && !this.wayIsReachable[way.getId()])
                .forEach(way -> {
                    examined[baseWayId][way.getId()] = true;

                    // Loop and not Stream, because it seems to be very much harder
                    // to stop inside of a Stream than to break a loop.
                    for(Coordinate node : way.getAllNodes()){
                        if( possibleIntersectionCoordinates.contains(node) ){
                            this.baseWaysQueue.add(way.getId());
                            this.wayIsReachable[way.getId()] = true;
                            break;
                        }
                    }
                });
    }

}
