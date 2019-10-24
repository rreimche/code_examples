/* sieht fertig aus */

package spiel;

import nutzer.*;
import org.junit.Before;
import org.junit.Test;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/**
 * Implementationsklasse fuer Spiel
 */
public class SpielImplTest {

    Spiel spiel;

    Spielstand spielstand;
    SpielpartieStub spielpartie;

    List<Nutzer> nutzer;
    List<SpielerStub> teilnehmer;
    List<SpielRObserverStub> observer;

    @Before
    public void init() throws Exception{

        /* erstelle Abhängigkeiten */
        nutzer = new ArrayList<>();
        teilnehmer = new ArrayList<>();
        observer = new ArrayList<>();
        spielstand = new SpielstandStub();
        for(int i=0; i<3; i++){
            nutzer.add( new NutzerStub("Nutzer" + (i+1), "testpass") );
            try {
                teilnehmer.add( new SpielerStub("Spieler" + (i+1) ) );
            } catch (RemoteException e) {
                e.printStackTrace();
            }
            teilnehmer.get(i).setNutzer(nutzer.get(i));
            observer.add( new SpielRObserverStub() );

        }

        ArrayList<Teilnehmer> teilnehmer_temp = new ArrayList<>(teilnehmer);

        spielpartie = new SpielpartieStub(teilnehmer_temp, spielstand);

        /*
         * erstelle eine List<SpielRObserver> für SpielFactory
         * wir brauchen das deswegen, weil SpielFactoryImpl.erstelleSpiel nur
         * List<SpielRObserver> bekommen kann, nicht List<SpielRObserverStub>
         */

        ArrayList<SpielRObserver> observer_tmp = new ArrayList<>(observer);

        /* erstelle spiel */
        spiel = SpielFactory.erstelleSpiel(
                teilnehmer_temp,
                nutzer,
                observer_tmp,
                spielstand,
                spielpartie,
                "Testspiel"
                );
    }

    /*@Test
    public void spielStartenGewinnerBestimmt() throws Exception{
        *//* spielstand <- ein Teilnehmer mit 7 GP, andere mit <7 *//*
        for (int i=0; i<3; i++) {
            spielstand.zufuegeGewinnpunkte(teilnehmer.get(i), 7 - i);
        }

        spiel.spielStarten();

        assertAlleObserverNotified("notifySpielBeendet");
    }

    @Test
    public void spielStartenGewinnerUnbestimmt()  throws Exception{
        *//* Spielstand <- niemand hat mehr als 6 GP *//*
        for (int i=0; i<3; i++) {
            spielstand.zufuegeGewinnpunkte(teilnehmer.get(i), 6 - i);
        }

        spiel.spielStarten();

        assertAlleObserverNotified("notifySpielBeendet");
    }

    @Test
    public void spielStartenErstePartie()  throws Exception{
        *//* spielstand <- keine GP wurden vergeben *//*

        spiel.spielStarten();

        assertAlleObserverNotified("notifySpielBeendet");
    }

    @Test
    public void spielStartenNur2TeilnehmerSpielBeendet()  throws Exception{
        teilnehmer.remove(0);
        observer.remove(0);

        *//* spielstand <- ein Teilnehmer hat 7 GP, der andere  <7 *//*
        for (int i=0; i<2; i++) {
            spielstand.zufuegeGewinnpunkte(teilnehmer.get(i), 7 - i);
        }

        spiel.spielStarten();

        assertAlleObserverNotified("notifySpielBeendet");
    }

    @Test
    public void spielStartenNur2TeilnehmerSpielNichtBeendet()  throws Exception{
        teilnehmer.remove(0);
        observer.remove(0);

        *//* Spielstand <- niemand hat mehr als 6 GP *//*
        for (int i=0; i<2; i++) {
            spielstand.zufuegeGewinnpunkte(teilnehmer.get(i), 6 - i);
        }

        spiel.spielStarten();

        assertAlleObserverNotified("notifySpielGebrochen");
    }*/

    /* test for calling spielpartie.spielpartieDurchfuehren() und nutzer.aktualisiereStatistik(gewinner)
     * und noch für SpielRObserver.notifySpielBeendet(spiel, spielstand) */
    @Test
    public void spielStarten() throws Exception{
        List<SpielRObserver> observerList = new ArrayList<>(observer);
        spiel.spielStarten(observerList);
        assertTrue("Spielpartie.spielpartieDurchfuheren() muss aufgerufen werden", spielpartie.obDurchgefuehrt());
        ArrayList<NutzerStub> temp = new ArrayList(nutzer);
        for(NutzerStub n : temp){
            assertTrue("Bei allen Nutzer muss Statistik aktualisiert werden", n.obStatistikAktualisiert());
        }
        assertAlleObserverNotified("notifySpielBeendet");
    }


    @Test
    public void notifyBereitzumDruecken()  throws Exception{
        spiel.notifyBereitzumDruecken();
        assertAlleObserverNotified("notifyBereitzumDruecken");
    }


    @Test
    public void notifyEndeSpielpartie() throws Exception{
        spiel.notifyEndeSpielpartie("teilnehmer0", 1, "teilnehmer1", 1, "teilnehmer2", 0);
        assertAlleObserverNotified("notifyEndeSpielpartie");
    }


    @Test
    public void notifyEndeStich() throws Exception{
        spiel.notifyEndeStich(teilnehmer.get(0).getName(), new ArrayList<>());
        assertAlleObserverNotified("notifyEndeStich");
    }


    @Test
    public void notifyKarteGelegt() throws Exception{
        spiel.notifyKarteGelegt(teilnehmer.get(0).getName(), new KarteImpl(Farbe.EIDEX, Symbol.ACHT));
        assertAlleObserverNotified("notifyKarteGelegt");
    }


    @Test
    public void notifyKarteVerteilt() throws Exception{
        spiel.notifyKarteVerteilt(new KarteImpl(Farbe.EIDEX, Symbol.ACHT), "TestNutzer");
        assertAlleObserverNotified("notifyKarteVerteilt");
    }


    @Test
    public void notifySpielBereit() throws Exception{
        spiel.notifySpielBereit(true);
        assertAlleObserverNotified("notifySpielBereit");
    }


    @Test
    public void notifySpielerAmZug() throws Exception{
        spiel.notifySpielerAmZug(teilnehmer.get(0));
        assertAlleObserverNotified("notifySpielerAmZug");
    }


    @Test
    public void notifyTrumpfSet() throws Exception{
        spiel.notifyTrumpfSet(new KarteImpl(Farbe.HERZ, Symbol.SIEBEN));
        assertAlleObserverNotified("notifyTrumpfSet");
    }


    @Test
    public void notifySpielGebrochen() throws Exception{
        spiel.notifySpielGebrochen(teilnehmer.get(0));
        assertAlleObserverNotified("notifySpielGebrochen");
    }


    @Test
    public void addAndRemoveObserver(){
        try {
            spiel.notifyBereitzumDruecken();
        } catch (RemoteException e) {
            e.printStackTrace();
        }

        assertAlleObserverNotified("notifyBereitzumDruecken");

        try{
            spiel.removeObserver(observer.get(0));
        } catch (RemoteException e){ System.out.println("RemoteException caught."); }

        try {
            spiel.notifySpielBereit(true);
        } catch (RemoteException e) {
            e.printStackTrace();
        }

        for(int i = 1; i<2; i++){
            assertTrue("Alle Observer sollen vom SpielBereit notified sein", observer.get(i).obNotified("notifySpielBereit"));
        }
        assertEquals("Der entfernte Observer soll nicht notified bleiben.", false, observer.get(0).obNotified("notifySpielBereit"));
    }

    @Test
    public void notifyLosGehts(){
        try {
            spiel.notifyLosGehts();
        } catch (RemoteException e) {
            e.printStackTrace();
        }
        assertAlleObserverNotified("notifyLosGehts");
    }

    @Test
    public void notifyShowKarteGedruecktAndere(){
        try {
            spiel.notifyShowKarteGedruecktAndere("Random String", new KarteImpl(Farbe.HERZ, Symbol.ASS));
        } catch (RemoteException e) {
            e.printStackTrace();
        }
        assertAlleObserverNotified("notifyShowKarteGedruecktAndere");
    }


    private void assertAlleObserverNotifiedSpielGebrochen() {
        SpielRObserverStub obs;
        Iterator<SpielRObserverStub> iterator = observer.iterator();
        while (iterator.hasNext()){
            obs = iterator.next();
            assertEquals("Alle Observer sollen vom Spielbruch notified sein", true, obs.obNotified("notifySpielGebrochen"));
        }
    }


    private void assertAlleObserverNotifiedSpielBeendet(){
        SpielRObserverStub obs;
        Iterator<SpielRObserverStub> iterator = observer.iterator();
        while (iterator.hasNext()){
            obs = iterator.next();
            assertEquals("Alle Observer sollen vom Spielende notified sein", true, obs.obNotified("notifySpielBeendet"));
        }
    }

    private void assertAlleObserverNotified(String functionsname){
        SpielRObserverStub obs;
        Iterator<SpielRObserverStub> iterator = observer.iterator();
        while (iterator.hasNext()){
            obs = iterator.next();
            switch(functionsname) {
                case "notifyBereitzumDruecken":
                    assertEquals("Alle Observer sollen vom BereitzumDruecken notified sein", true, obs.obNotified("notifyBereitzumDruecken"));
                    break;
                case "notifyEndeSpielpartie":
                    assertEquals("Alle Observer sollen vom EndeSpielpartie notified sein", true, obs.obNotified("notifyEndeSpielpartie"));
                    break;
                case "notifyEndeStich":
                    assertEquals("Alle Observer sollen vom EndeStich notified sein", true, obs.obNotified("notifyEndeStich"));
                    break;
                case "notifyKarteGelegt":
                    assertEquals("Alle Observer sollen vom KarteGelegt notified sein", true, obs.obNotified("notifyKarteGelegt"));
                    break;
                case "notifyKarteVerteilt":
                    assertEquals("Alle Observer sollen vom KarteVerteilt notified sein", true, obs.obNotified("notifyKarteVerteilt"));
                    break;
                case "notifySpielBereit":
                    assertEquals("Alle Observer sollen vom SpielBereit notified sein", true, obs.obNotified("notifySpielBereit"));
                    break;
                case "notifySpielerAmZug":
                    assertEquals("Alle Observer sollen vom SpielerAmZug notified sein", true, obs.obNotified("notifySpielerAmZug"));
                    break;
                case "notifyTrumpfSet":
                    assertEquals("Alle Observer sollen vom TrumpfSet notified sein", true, obs.obNotified("notifyTrumpfSet"));
                    break;
                case "notifySpielBeendet":
                    assertEquals("Alle Observer sollen vom SpielBeendet notified sein", true, obs.obNotified("notifySpielBeendet"));
                    break;
                case "notifySpielGebrochen":
                    assertEquals("Alle Observer sollen vom SpielGebrochen notified sein", true, obs.obNotified("notifySpielGebrochen"));
                    break;
                case "notifyLosGehts":
                    assertTrue("Alle Observer sollen vom losGehts notified sein", obs.obNotified("notifyLosGehts"));
                    break;
                case "notifyShowKarteGedruecktAndere":
                    assertTrue("Alle Observer sollen vom ShowKarteGedruecktAndere notified sein.", obs.obNotified("notifyShowKarteGedruecktAndere"));
                    break;
                default:
                    System.out.println("Wrong function name:" + functionsname);
                    fail();
            }
        }
    }

}
