package spiel;

import exceptions.KarteNichtVorhandenException;
import exceptions.KarteSchonVorhandenException;

import java.util.ArrayList;
import java.util.Collections;

/**
 * Implementationsklasse für Blatt
 */
public class BlattImpl implements Blatt {

    private final ArrayList<Karte> blatt = new ArrayList<>();
    private final KartenComparator comparator;

    public BlattImpl(KartenComparator c){
        comparator = c;
    }

    @Override
    public ArrayList<Karte> getBlatt() {
        return blatt;
    }

    @Override
    public void karteEntfernen(Karte karte) throws KarteNichtVorhandenException {
        for (int i = 0; i < blatt.size(); i++){
            if (blatt.get(i).getFarbe() == karte.getFarbe() && blatt.get(i).getSymbol() == karte.getSymbol()){
                blatt.remove(i);
                return;
            }
        }

        throw new KarteNichtVorhandenException();
        /*
        L�uft nicht wegen contains, was die IDs vergleicht
        if (!blatt.contains(karte)){
            throw new KarteNichtVorhandenException();
        }
        blatt.remove(karte);
        */
    }

    @Override
    public void karteZufuegen(Karte karte) throws KarteSchonVorhandenException {
        for (Karte aBlatt : blatt) {
            if ((karte.getFarbe() == aBlatt.getFarbe()) && (karte.getSymbol() == aBlatt.getSymbol())) {
                throw new KarteSchonVorhandenException();
            }
        }
        blatt.add(karte);
    }

    @Override
    public void sortiereBlattFarblichAufsteig() {
        Collections.sort(blatt, comparator);
    }

    @Override
    public void sortiereBlattFarblichAbsteig() {
        Collections.sort(blatt, comparator);
        Collections.reverse(blatt);
    }
}
