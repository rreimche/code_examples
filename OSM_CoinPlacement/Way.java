package core;

import org.json.simple.JSONArray;

import java.util.Arrays;
import java.util.List;

/*
 * Represents an OSM way.
 *
 * @author rreimche
 */
public class Way {
    int id;
    Coordinate[] nodes;

    /*
     * @param id a unique id of the way
     * @param anotherWay node coordinates that constitute the way
     */
    public Way(int id, Coordinate[] anotherWay){
        this.id = id;
        this.nodes = new Coordinate[anotherWay.length];

        for(int i = 0; i < anotherWay.length ; i++){
            nodes[i] = anotherWay[i];
        }
    }

    /*
     * Getter for id.
     */
    public int getId(){
        return id;
    }

    /*
     * Getter for node coordinate at certain position in the sequence of nodes forming the way.
     *
     * @param i index of the node in the way
     * @return node coordinate at i
     */
    public Coordinate getNodeAtPosition(int i){
        return nodes[i];
    }

    /*
     * Getter for all the node coordinates of the way.
     *  @return coordinates of all the nodes constituting the way
     */
    public Coordinate[] getAllNodes(){
        return nodes;
    }

    /*
     * Checks if the way contains a given node.
     *
     * @param anotherNode node to check for
     */
    public boolean nodeIsInWay(Coordinate anotherNode){
        for(Coordinate node : nodes){
            if( node.equals(anotherNode) ) return true;
        }

        return false;
    }

    /*
     * Returns a representation of the Way, where all coordinates are represented by a pair of Strings
     * packed inside a JSONArray.
     *
     * @return JSONArray<JSONArray<String>> representation of the node
     */
    public JSONArray toJSONArrayWithStrings(){
        JSONArray result = new JSONArray();
        Arrays.stream(this.nodes)
                .forEach(node -> {
                    String lat = Double.toString(node.getLatitude());
                    String lon = Double.toString(node.getLongitude());
                    JSONArray nodeCoords = new JSONArray();
                    nodeCoords.add(lat);
                    nodeCoords.add(lon);
                    result.add(nodeCoords);
                });

        return result;
    }

    /*
     * Returns a representation of the Way, where all coordinates are represented by a pair of doubles
     * packed inside a JSONArray.
     *
     * @return JSONArray<JSONArray<double>> representation of the node
     */
    public JSONArray toJSONArrayWithDoubles(){
        JSONArray result = new JSONArray();
        Arrays.stream(this.nodes)
                .forEach(node -> {
                    double lat = node.getLatitude();
                    double lon = node.getLongitude();
                    JSONArray nodeCoords = new JSONArray();
                    nodeCoords.add(lat);
                    nodeCoords.add(lon);
                    result.add(nodeCoords);
                });

        return result;
    }


    /*
     * Getter for the length of the way defined as the number of nodes
     * that constitute the way.
     */
    public int length(){ return nodes.length;}

}
