package spiel;

import com.sun.istack.internal.Nullable;
import exceptions.KarteUnlegbarException;
import exceptions.SpielerNichtAmZugException;
import exceptions.StichstapelUnerwartetLeerException;
import javafx.util.Pair;
import nutzer.Teilnehmer;

import java.rmi.RemoteException;
import java.util.*;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Implementationsklasse für Stichstapel.
 */
public class StichstapelImpl implements Stichstapel {
    @Nullable
    private Karte trumpf;
    private List<Pair<Teilnehmer, Karte>> stichstapel = new ArrayList<>();
    private Spielstand spielstand;

    public StichstapelImpl(){}
    public StichstapelImpl(Karte t, Spielstand s){
        trumpf = null;
        spielstand = s;
    }

    @Override
    public Teilnehmer abschliesseStich() throws StichstapelUnerwartetLeerException {
        Teilnehmer stichgewinner = stichgewinnerBestimmen();
        for( Pair<Teilnehmer, Karte> k : stichstapel){
            try {
                spielstand.zufuegeStichpunkte(stichgewinner, k.getValue());
            } catch (RemoteException e) {
                e.printStackTrace();
            }
        }
        return stichgewinner;
    }


    @Override
    public Teilnehmer abschliesseLetztenStich() throws StichstapelUnerwartetLeerException{
        Teilnehmer stichgewinner = abschliesseStich();
        try {
            spielstand.zufuegeStichpunkteLetzterStich(stichgewinner);
        } catch (RemoteException e) {
            e.printStackTrace();
        }
        return stichgewinner;
    }

    @Override
    public void aendereSpielstand(Teilnehmer teilnehmer, Karte karte) {
        try {
            spielstand.zufuegeStichpunkte(teilnehmer, karte);
        } catch (RemoteException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void drueckeKarte(Teilnehmer teilnehmer, Karte karte){
        try {
            spielstand.zufuegeDrueckStichpunkte(teilnehmer, karte);
        } catch (RemoteException e) {
            e.printStackTrace();
        }
    }

    @Override
    public Karte getTrumpf() {
        return trumpf;
    }

    @Override
    public void legeKarte(Karte karte, Teilnehmer teilnehmer, Teilnehmer spielerAnReihe) throws KarteUnlegbarException, SpielerNichtAmZugException{
        try {
            if(!(spielerAnReihe.getName().equals(teilnehmer.getName()))) throw new SpielerNichtAmZugException();
        } catch (RemoteException e) {
            e.printStackTrace();
        }
        try {
            if( !obKarteLegbar(karte, teilnehmer.getBlatt().getBlatt()) ) throw new KarteUnlegbarException();
        } catch (RemoteException e) {
            e.printStackTrace();
        }
        stichstapel.add(new Pair<>(teilnehmer, karte));
    }

    @Override
    public void resetStichstapel() {
        stichstapel = new ArrayList<>();
    }

    @Override
    public void setTrumpf(Karte karte) {
       trumpf = karte;
    }

    @Override
    public List<Karte> getGelegteKarten(){
        List<Karte> result = new ArrayList<>();
        for(Pair<Teilnehmer,Karte> k : stichstapel){
            result.add(k.getValue());
        }

        Collections.sort(result, new KartenComparator(trumpf));

        return result;
    }

    private Teilnehmer stichgewinnerBestimmen() throws StichstapelUnerwartetLeerException {
        KartenComparator kartenComparator = new KartenComparator(trumpf);
        Pair<Teilnehmer,Karte> stichgewinner = stichstapel.get(0);

        if(kartenComparator.compare(stichgewinner.getValue(),stichstapel.get(1).getValue()) == -1){
            stichgewinner = stichstapel.get(1);
        }
        if(kartenComparator.compare(stichgewinner.getValue(), stichstapel.get(2).getValue()) == -1){
            stichgewinner = stichstapel.get(2);
        }
        return stichgewinner.getKey();
        /*
       // if( stichstapel.isEmpty() ) throw new StichstapelUnerwartetLeerException();


        //init
        Pair<Teilnehmer,Karte> max;
        Farbe kartenfarbe, firstfarbe = stichstapel.get(0).getValue().getFarbe();
        KartenComparator kartenComperator = new KartenComparator(trumpf);
        List<Pair<Teilnehmer, Karte>> stapel_temp = new ArrayList<>(stichstapel);
        List<Karte> karten = new ArrayList<>();
        ArrayList<Pair<Teilnehmer, Karte>> entriesToRemove = new ArrayList<>();

        //remove all cards that are neither trumpf-color nor first-color (color of the first card placed into stichstapel)
        //because they cannot win in any case.
        for(Pair<Teilnehmer, Karte> entry : stapel_temp){
            karten.add(entry.getValue()); //fill the array for later use
            if( stapel_temp.indexOf(entry) == 0 ) continue; //if it's the first entry, skip
            kartenfarbe = entry.getValue().getFarbe();
            //if this is the case, mark it to remove later
            if( !kartenfarbe.equals(trumpf.getFarbe()) && !kartenfarbe.equals(firstfarbe))
                entriesToRemove.add(entry);
        }

        //remove all the marked entries
        stapel_temp.removeAll(entriesToRemove);

        //find the max
        max = stapel_temp.get(0);
        Karte currentKarte, maxKarte;
        maxKarte = max.getValue();
        for(Pair<Teilnehmer, Karte> entry : stapel_temp){
            currentKarte = entry.getValue();
            //if current card higher max then it's max. so in the end we've stepped through all the cards and have the real max.
            if( kartenComperator.compare(currentKarte, maxKarte) == 1 ) {
                maxKarte = currentKarte;
                max = entry;
            }
        }

        return max.getKey();*/


        /*
        if(kartenComperator.compare(stapel_temp.get(0).getValue(), stapel_temp.get(1).getValue()) == -1){
            if(kartenComperator.compare(stapel_temp.get(1).getValue(), stapel_temp.get(2).getValue()) == -1){
                max = stapel_temp.get(2);
            }
            else {
                max = stapel_temp.get(1);
            }
        }
        else {
            if(kartenComperator.compare(stapel_temp.get(0).getValue(), stapel_temp.get(2).getValue()) == -1){
                max = stapel_temp.get(2);
            }
            else {
                max = stapel_temp.get(0);
            }
        }


        return max.getKey();*/
    }


    @Override
    public List<Karte> getLegbareKarten(List<Karte> karteList2){
        List<Karte> karteList = new ArrayList<>(karteList2);
        if(stichstapel.size() > 0) {
            if (trumpf.getSymbol() == Symbol.ASS || trumpf.getSymbol() == Symbol.SECHS) {
                Karte ersteKarte = stichstapel.get(0).getValue();
                if (obEnthaeltFarbe(karteList, ersteKarte.getFarbe())) {
                    karteList = entferneAlleanderenFarben(karteList, ersteKarte.getFarbe());
                }
            }
            else {
                Karte ersteKarte = stichstapel.get(0).getValue();
                List<Karte> tempList = new ArrayList<>(karteList);
                tempList.remove(new KarteImpl(trumpf.getFarbe(), Symbol.BUBE));
                if (obEnthaeltFarbe(tempList, ersteKarte.getFarbe())) {
                    karteList = entferneAlleanderenFarben(karteList, ersteKarte.getFarbe());
                    if (stichstapel.size() > 1) {
                        Karte zweiteKarte = stichstapel.get(1).getValue();
                        if (!ersteKarte.getFarbe().equals(trumpf.getFarbe()) && zweiteKarte.getFarbe().equals(trumpf.getFarbe())) {
                            for (Karte k : niedrigereTrumpfkarten(karteList, zweiteKarte)) {
                                karteList.remove(k);
                            }
                        }
                    }
                } else {
                    if (stichstapel.size() > 1) {
                        Karte zweiteKarte = stichstapel.get(1).getValue();
                        if (ersteKarte.getFarbe().equals(trumpf.getFarbe()) && zweiteKarte.getFarbe().equals(trumpf.getFarbe())) {
                            tempList = new ArrayList<>(niedrigereTrumpfkarten(karteList, zweiteKarte));
                            if (karteList.size() != tempList.size()) {
                                for (Karte k : tempList) {
                                    karteList.remove(k);
                                }
                            }
                        }
                    }
                }
            }
        }
        return karteList;
    }

    //Gibt true zurueck, falls karte legbar ist, sonst false
    @Override
    public boolean obKarteLegbar(Karte karte, List<Karte> karteList){
        List<Karte> legbareKarten = getLegbareKarten(karteList);
        for (Karte legbareKarte : legbareKarten){
            if ((legbareKarte.getFarbe() == karte.getFarbe()) && (legbareKarte.getSymbol() == karte.getSymbol())){
                return true;
            }
        }
        return false;
    }

    //gibt alle Trumpfkarten zur�ck, die einen niedrigeren rang haben als karte
    private List<Karte> niedrigereTrumpfkarten(List<Karte> karteList, Karte karte){
        KartenComparator kartenComperator= new KartenComparator(trumpf);
        List<Karte> tempList = new ArrayList<>();
        for (Karte k : karteList){
            if(k.getFarbe().equals(trumpf.getFarbe()) && kartenComperator.compare(karte,k) == 1){
                tempList.add(k);
            }
        }
        return tempList;
    }

    //Entfernt alle Karten, die nicht die Farbe farbe und nicht die Trumpffarbe haben
    private List<Karte> entferneAlleanderenFarben(List<Karte> karteList2,Farbe farbe){
        List<Karte> karteList = new CopyOnWriteArrayList<>(karteList2);
        if (!(trumpf.getSymbol() == Symbol.ASS || trumpf.getSymbol() == Symbol.SECHS)){
            for (Karte k : karteList){
                if(!k.getFarbe().equals(farbe) && !k.getFarbe().equals(trumpf.getFarbe())){
                    karteList.remove(k);
                }
            }
        }
        else {
            for (Karte k : karteList){
                if(!k.getFarbe().equals(farbe)){
                    karteList.remove(k);
                }
            }
        }
        return karteList;
    }



    private boolean obEnthaeltFarbe(List<Karte> karteList,Farbe farbe){
        for (Karte k : karteList){
            if(k.getFarbe().equals(farbe)){
                return true;
            }
        }
        return false;
    }

}
