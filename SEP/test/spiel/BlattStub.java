package spiel;

import java.util.ArrayList;
import java.util.List;
import java.util.SortedSet;

/**
 * Created by rreimche on 23.06.15.
 */
public class BlattStub implements Blatt {
    List<Karte> karten = new ArrayList<>();

    @Override
    public ArrayList<Karte> getBlatt() {
        return null;
    }

    @Override
    public void karteEntfernen(Karte karte) {

    }

    @Override
    public void karteZufuegen(Karte karte) {
        karten.add(karte);
    }

    @Override
    public void sortiereBlattFarblichAbsteig() {

    }

    @Override
    public void sortiereBlattFarblichAufsteig() {

    }

    public boolean obKartenVerteilt(){
        if ( karten.size() == 12 ) return true;
        return false;
    }

}
