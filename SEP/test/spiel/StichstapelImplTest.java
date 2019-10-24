/* sieht fertig aus */

package spiel;

import nutzer.Bot;
import nutzer.Teilnehmer;
import org.junit.Before;
import org.junit.Test;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * Created by rreimche on 12.06.15.
 */
public class StichstapelImplTest {
    Stichstapel stichstapel;
    Spielstand spielstand;
    Teilnehmer teilnehmer;
    Karte karte;

    @Before
    public void init(){
        spielstand = new SpielstandStub();
        stichstapel = new StichstapelImpl(new KarteImpl(Farbe.HERZ, Symbol.SIEBEN), spielstand);

        try {
            teilnehmer = new Bot("TestBot");
        } catch (RemoteException e) {
            e.printStackTrace();
        }
        karte = new KarteImpl(Farbe.EIDEX, Symbol.ACHT);
    }


    /* erstelle Spielstand, erstelle Karten in Stichstapel,
        durchführe abschließeStich(), prüfe ob an Spielstand zufuegeStichpunkte() mit richtigen parametern
        aufgerufen wurde und ob der wirklich gewonnene Teilnehmer zurückgegeben ist.
     *//*
    public void stichAbschließen() {
    }*/

    @Test
    public void aendereSpielstand()  throws Exception{
        spielstand.setTrumpf(karte);
        stichstapel.aendereSpielstand(teilnehmer, karte );
        assertEquals("Alles wird an Spielstand übergeben:", 0, spielstand.getStichpunkte(teilnehmer));
    }

    @Test
    public void testObKarteLegbar() {
        stichstapel.setTrumpf(new KarteImpl(Farbe.HERZ, Symbol.SIEBEN));
        List<Karte> karteList = new ArrayList<>();
        karteList.add(new KarteImpl(Farbe.RABE,Symbol.ACHT));
        karteList.add(new KarteImpl(Farbe.RABE, Symbol.NEUN));
        karteList.add(new KarteImpl(Farbe.HERZ, Symbol.ACHT));
        assertTrue(stichstapel.obKarteLegbar(new KarteImpl(Farbe.RABE, Symbol.ACHT), karteList));
    }

    @Test
    public void testObKarteLegbar2() {
        stichstapel.setTrumpf(new KarteImpl(Farbe.HERZ, Symbol.SIEBEN));
        List<Karte> karteList = new ArrayList<>();
        karteList.add(new KarteImpl(Farbe.RABE,Symbol.ACHT));
        karteList.add(new KarteImpl(Farbe.RABE,Symbol.NEUN));
        karteList.add(new KarteImpl(Farbe.HERZ, Symbol.ACHT));
        for (Karte k : stichstapel.getLegbareKarten(karteList)){
            System.out.println(k.getFarbe().toString() + " " + k.getSymbol().toString()+"\n");
        }
    }


    /* trivial
    public void getTrumpf() {

    }
    */

    /*trivial
    public void legeKarte(Karte k, Teilnehmer t) {
    }
    */

    /* zu einfach zu testen*
    public void resetStichstapel() {

    }*/

    /* trivial
    public void setTrumpf() {

    }*/
}
