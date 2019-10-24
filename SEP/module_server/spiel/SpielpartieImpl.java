/* sieht fertig aus */

package spiel;
import exceptions.StichstapelUnerwartetLeerException;
import nutzer.Bot;
import nutzer.Teilnehmer;

import java.rmi.RemoteException;
import java.util.*;
import java.util.concurrent.CountDownLatch;

/**
 * Implementationsklasse für Spielpartie
 */
public class SpielpartieImpl implements Spielpartie {
    private final List<Teilnehmer> teilnehmerList = new ArrayList<>();
    private final Spielstand spielstand;
    private final Stichstapel stichstapel;
    private final Spiel spiel;
    private int partienr, stichnummer;
    private Karte trumpf;
    private Teilnehmer spielerAnReihe;

    public SpielpartieImpl(List<Teilnehmer> tl, Spielstand spielstand, Spiel spiel){
        teilnehmerList.addAll(tl);
        this.spielstand = spielstand;
        this.spiel = spiel;
        stichstapel = new StichstapelImpl(null, spielstand);
        stichnummer = 1;
        try {
            for (Teilnehmer teilnehmer : teilnehmerList) {
                teilnehmer.setStichstapel(stichstapel);
            }
        } catch(RemoteException e){ e.printStackTrace(); }
    }

    public SpielpartieImpl(List<Teilnehmer> tl, Spielstand ss, Spiel sp, Stichstapel st){
        teilnehmerList.addAll(tl);
        spielstand = ss;
        spiel = sp;
        stichstapel = st;
        stichnummer = 1;
        try {
            for (Teilnehmer t : teilnehmerList) {
                t.setStichstapel(stichstapel);
            }
        }catch(RemoteException e){ e.printStackTrace(); }
    }

    /*@Override
    public int getPartieNr() {
        return partienr;
    }*/

    @Override
    public Karte getTrumpf() {
        return trumpf;
    }

    @Override
    public void spielpartieDurchfueren() {
        verteileKarten();
        notifyBereitZumDruecken();
        notifyLosGehts();
        wechsleSpieler();
        for(int i = 0; i < 11; i++) {
            durchfuehreStich();
        }
        gewinnpunkteVerteilen();
        try {
            Thread.sleep(2000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        try {
            spielstand.resetSpielpartie();
        } catch (RemoteException e) {
            e.printStackTrace();
        }
    }

    @Override
    public Teilnehmer getSpielerAnReihe() {
        return spielerAnReihe;
    }

    @Override
    public void aktualisiereSpielerListe(String zuersetzenderTeilnehmer, Teilnehmer t2) throws RemoteException {
        for (int i = 0; i < teilnehmerList.size(); i++){
            if (teilnehmerList.get(i).getName().equals(zuersetzenderTeilnehmer)){
                teilnehmerList.remove(i);
                teilnehmerList.add(i, t2);
            }
        }
    }

    private void verteileKarten() {
        List<Karte> deck = new ArrayList<>();
        for(Farbe f : Farbe.values()) {
            for(Symbol s: Symbol.values()) {
                deck.add(new KarteImpl(f, s));
            }
        }
        Collections.shuffle(deck);
        wechsleSpieler();
        int i = 0;                              //nummer der verteilten Karte (0 bis 3)
        while(deck.size() > 1){
            gebeKarte(deck.get(0), spielerAnReihe);
            deck.remove(0);
            i++;

            //wenn die 4. Karte einem Teilnehmer verteilt wurde, wechsleSpieler.
            if( i == 4 ){
//                try {
//                    Thread.sleep(200);
//                } catch (InterruptedException e) {
//                    e.printStackTrace();
//                }
                wechsleSpieler();
                i = 0;
            }
        }
        setTrumpfKarte(deck.get(0));
        try {
            spiel.notifyTrumpfSet(trumpf);
        } catch (RemoteException e) {
            e.printStackTrace();
        }
        gebeKarte(deck.get(0), spielerAnReihe);
        deck.remove(0);
    }

    private void wechsleSpieler() {
        if ( spielerAnReihe == null ) {
            spielerAnReihe = teilnehmerList.get(0);
        } else {
            int i = teilnehmerList.indexOf(spielerAnReihe);
            if ( i == 2 ) {
                spielerAnReihe = teilnehmerList.get(0);
            } else {
                ++i;
                spielerAnReihe = teilnehmerList.get(i);
            }
        }
    }

    private void wechsleSpieler(Teilnehmer stichgewinner){
        spielerAnReihe = stichgewinner;
    }

    private void gebeKarte(Karte karte, Teilnehmer teilnehmer){
        try{
            teilnehmer.karteNehmen(karte);
            spiel.notifyKarteVerteilt(karte, teilnehmer.getName());
        } catch (RemoteException e) {
            e.printStackTrace();
        }

    }

    private void setTrumpfKarte(Karte k){
        // !!!kopiere!!! die letzte nicht verteilte Karte als trumpf-karte
        trumpf = new KarteImpl(k.getFarbe(), k.getSymbol());

        stichstapel.setTrumpf(trumpf);
        try {
            spielstand.setTrumpf(trumpf);
        } catch (RemoteException e) {
            e.printStackTrace();
        }
        /*
        try {
            spiel.notifyTrumpfSet(trumpf);
        } catch (RemoteException e) {
            e.printStackTrace();
        }*/
    }

    private void notifyBereitZumDruecken(){
        try {
            spiel.notifyBereitzumDruecken();
        } catch (RemoteException e) {
            e.printStackTrace();
        }
    }

    private void notifyLosGehts(){
        try {
            spiel.notifyLosGehts();
        } catch (RemoteException e) {
            e.printStackTrace();
        }
    }

    private void durchfuehreStich(){
        Teilnehmer stichgewinner = null;
        stichstapel.resetStichstapel();
        durchfuehreZuege();
        //Hier wird gewartet
        try {
            if (stichnummer != 11) {
                stichgewinner = stichstapel.abschliesseStich();
            } else {
                stichgewinner = stichstapel.abschliesseLetztenStich();
            }
            wechsleSpieler(stichgewinner);

            try {
                List<String> teilnehmerStichpunkteList = new ArrayList<>();
                for(Teilnehmer t : teilnehmerList){
                    teilnehmerStichpunkteList.add(t.getName());
                    if (stichnummer != 11) {
                        teilnehmerStichpunkteList.add(Integer.toString(spielstand.getStichpunkte(t)));
                    } else {
                        teilnehmerStichpunkteList.add(Integer.toString(spielstand.getStichpunkte(t)+spielstand.getDrueckStichpunkte(t)));
                    }
                    teilnehmerStichpunkteList.add(Integer.toString(spielstand.getStichpunkte(t)));
                }
                spiel.notifyEndeStich(stichgewinner.getName(),teilnehmerStichpunkteList);
            } catch (RemoteException e) {
                e.printStackTrace();
            }
        } catch (StichstapelUnerwartetLeerException e) { e.printStackTrace(); }
        stichnummer++;
    }

    private void durchfuehreZuege(){
        for(int i = 0; i < 3; i++){
            if(!(spielerAnReihe instanceof Bot)){       //Wenn realer Spieler
                try {
                    spiel.notifySpielerAmZug(spielerAnReihe);
                } catch (RemoteException e) {
                    e.printStackTrace();
                }
            }
            else{                                       //Bot ist an der Reihe
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                ((Bot)spielerAnReihe).karteAusspielen();
            }
            wechsleSpieler();
        }
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    private void gewinnpunkteVerteilen(){
        int ergebnistyp = 2;  //sehe die Spielregeln. Anstatt 1..4 haben wir hier 0..3.

        // um mit <Teilnehmer, stichpunkte> einfach operieren zu können,
        // erstelle und fülle eine ArrayList<Map.Entry<Teilnehmer, Integer>>
        List<Map.Entry<Teilnehmer, Integer>> stichpunkte = new ArrayList<>();
        int intReichste, intArmste; //die Indexen von Teilnehmer mit den meisten und wenigsten Stichpunkten in stichpunkte

        for(Teilnehmer t : teilnehmerList){
            try {
                stichpunkte.add(new AbstractMap.SimpleEntry<>(t, spielstand.getStichpunkte(t)+spielstand.getDrueckStichpunkte(t)));
            } catch (RemoteException e) {
                e.printStackTrace();
            }
        }


        //finde denReichste und denArmste
        intReichste = intArmste = 0; //init
        for(int i = 1; i < 3; i++){
            if( stichpunkte.get(intReichste).getValue() < stichpunkte.get(i).getValue() ) {
                intReichste = i;
            } else if ( stichpunkte.get(intArmste).getValue() > stichpunkte.get(i).getValue() ) {
                intArmste = i;
            }
        }

        if ( stichpunkte.get(intReichste).getValue() == 157 ) {
            ergebnistyp = 0;
            try {
                spielstand.zufuegeGewinnpunkte(stichpunkte.get(intReichste).getKey(), 2);
            } catch (RemoteException e) {
                e.printStackTrace();
            }
            //2nd type
        } else if ( stichpunkte.get(intReichste).getValue() >= 100 ) {
            ergebnistyp = 1;
            stichpunkte.remove(stichpunkte.get(intReichste));
            for(Map.Entry<Teilnehmer, Integer> e : stichpunkte){
                try {
                    spielstand.zufuegeGewinnpunkte(e.getKey(), 1);
                } catch (RemoteException e1) {
                    e1.printStackTrace();
                }
            }
        }

        if( ergebnistyp == 2) {
            //finde, ob die 4. Ergebnistyp vorliegt und eventuell zufuegeGewinnpunkte
            try {
                if (Objects.equals(stichpunkte.get(0).getValue(), stichpunkte.get(1).getValue())) {
                    spielstand.zufuegeGewinnpunkte(stichpunkte.get(2).getKey(), 2);
                    ergebnistyp = 3;
                } else if (Objects.equals(stichpunkte.get(1).getValue(), stichpunkte.get(2).getValue())) {
                    spielstand.zufuegeGewinnpunkte(stichpunkte.get(0).getKey(), 2);
                    ergebnistyp = 3;
                } else if (Objects.equals(stichpunkte.get(2).getValue(), stichpunkte.get(0).getValue())) {
                    spielstand.zufuegeGewinnpunkte(stichpunkte.get(1).getKey(), 2);
                    ergebnistyp = 3;
                }
            } catch (RemoteException e) {
                e.printStackTrace();
            }
        }
        //if it's still the 3rd type of Ergebnis, zufuegeGewinnpunkte
        if( ergebnistyp == 2) {
            try {
                spielstand.zufuegeGewinnpunkte(stichpunkte.get(intReichste).getKey(), 1);
                spielstand.zufuegeGewinnpunkte(stichpunkte.get(intArmste).getKey(), 1);
            } catch (RemoteException e) {
                e.printStackTrace();
            }
        }

        try {
            Teilnehmer teilnehmer0 = teilnehmerList.get(0);
            Teilnehmer teilnehmer1 = teilnehmerList.get(1);
            Teilnehmer teilnehmer2 = teilnehmerList.get(2);
            spiel.notifyEndeSpielpartie(teilnehmer0.getName(), spielstand.getGewinnpunkte(teilnehmer0), teilnehmer1.getName(), spielstand.getGewinnpunkte(teilnehmer1), teilnehmer2.getName(), spielstand.getGewinnpunkte(teilnehmer2));
        } catch (RemoteException e) {
            e.printStackTrace();
        }
    }
}
