package core;

import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import org.json.simple.JSONArray;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;


// TODO Consider using WGS84Point from ch.hsr.geohash instead of this class
/*
 * Represents a point on the earth map defined by latitude and longitude.
 * @author rreimche
 */
public class Coordinate {
    private double longitude;
    private double latitude;

    // The simples constructor.
    public Coordinate() {
    }

    /*
     * A more handy constructor.
     *
     * @param latitude
     * @param longitude
     */
    public Coordinate(double latitude, double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
    }

    /*
     * Another handy constructor.
     *
     * @param arr must be either JSONArray with 2 Doubles or with 2 Strings that
     *                      that represent the corresponding latitude and longitude respectfully.
     *
     * @throws Exception meaning arr does not pass.
     */
    public Coordinate(JSONArray arr) throws Exception {
        if( arr.size() > 2 ){ throw new Exception("Bad argument for Coordinate(JSONArray)"); };

        Object param1 = arr.get(0);
        Object param2 = arr.get(1);

        if(param1.getClass().equals(String.class) && param2.getClass().equals(String.class) ){

            this.latitude =  Double.valueOf((String) arr.get(0));
            this.longitude = Double.valueOf((String) arr.get(1));

        } else if (param1.getClass().equals(Double.class) && param2.getClass().equals(Double.class)){

            this.latitude =  (double) arr.get(0);
            this.longitude = (double) arr.get(1);

        } else throw new Exception("Bad type of argument for Coordinate(JSONArray)");

    }

    /*
     * Getter for the longitude.
     */
    public double getLongitude() {
        return longitude;
    }

    /*
     * Setter for the longitude.
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    /*
     * Getter for the latitude.
     */
    public double getLatitude() {
        return latitude;
    }

    /*
     * Setter for the latitude.
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }


    /*
     * @return TODO
     */
    public AttributeValue toAttributeValue() {
        Map<String, AttributeValue> values = new HashMap<>();
        values.put("longitude", new AttributeValue().withN(Double.toString(longitude)));
        values.put("latitude", new AttributeValue().withN(Double.toString(latitude)));
        return new AttributeValue().withM(values);
    }

    /*
     * Implements equality testing by comparison with a given object.
     *
     * @param o another Coordinate.
     * @return true if the latitudes and longitudes of both objects are equal
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Coordinate that = (Coordinate) o;
        return Double.compare(that.longitude, longitude) == 0 &&
                Double.compare(that.latitude, latitude) == 0;
    }

    /*
     * @return hash of this Coordinate
     */
    @Override
    public int hashCode() {
        return Objects.hash(longitude, latitude);
    }
}
