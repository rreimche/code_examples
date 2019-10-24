package core;

import java.nio.ByteBuffer;
import java.util.TreeMap;
import java.util.function.Function;
import ch.hsr.geohash.GeoHash;

/*
 * The main purpose of this class is
 * to check if a certain Coordinate is already added
 * to an instance of this class.
 *
 * Usage:
 * 1. Fill an instance with some Coordinates. They become possible intersection coordinates then.
 * 2. Test other Coordinates against the instance. If it contains the coordinate that is tested,
 *    then there is an intersection of two ways at this point.
 *
 * It is called "...Mapping", because it uses a mapping from a Coordinate to a double using GeoHash,
 * which allows contains(Coordinate coordinate) to work faster. TODO Rename?
 *
 * @author rreimche
 */
public class CoordsToPossibleIntersectionMapping {

    // Stores boolean values in a red-black tree.
    // Coould be an array of booleans but it would be of an immense size
    // because we don't have a maximum size for the play area which means the whole world is
    // the maximum :) And for that, according to my calculations, we'd need ~64000 Petabytes of storage :)
    // That's why we will enjoy logarithmic time complexity instead of constant :)
    private class Storage {

        private TreeMap<Double,Boolean> data = new TreeMap<>();

        public boolean find(Double index){ return data.get(index); }
        public boolean contains(Double index){ return  data.containsKey(index); }

        public void insert(Double index){ data.put(index, true); }
    }

    private Storage storage = new Storage();

    /*
     * Adds a Coordinate to Storage.
     *
     * @param coordinate
     *
     */
    public void add(Coordinate coordinate){
        storage.insert(getIndexFor(coordinate));
    }

    /*
     * Check if Storage contains a given Coordinate.
     *
     * @param coordinate
     *
     */
    public boolean contains(Coordinate coordinate){
        return storage.contains(getIndexFor(coordinate));
    }

    /*
     * Maps a Coordinate to a double using GeoHash.
     *
     * @param coordinate
     *
     * @return index of the coordinate in the Storage
     */
    private double getIndexFor(Coordinate coordinate){
        GeoHash geohash = GeoHash.withCharacterPrecision(coordinate.getLatitude(), coordinate.getLongitude(), 12);
        return (double) geohash.longValue();
    }

}