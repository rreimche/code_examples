package spiel;

import nutzer.Teilnehmer;
import nutzer.TeilnehmerFactory;
import nutzer.TeilnehmerStub;
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
public class SpielpartieImplTest {
    Spielpartie spielpartie;
    SpielStub spiel;
    List<BlattStub> blaetter;
    StichstapelStub stichstapel;
    SpielstandStub spielstand;
    List<TeilnehmerStub> teilnehmer;

    @Before
    public void init(){

        spielstand = new SpielstandStub();
        blaetter = new ArrayList<>();
        teilnehmer = new ArrayList<>();
        try {
            for (int i = 0; i < 3; i++) {
                blaetter.add(new BlattStub());
                teilnehmer.add(new TeilnehmerStub("TestTeilnehmer"));
            }
        } catch (RemoteException e){ e.printStackTrace(); }

        stichstapel = new StichstapelStub(teilnehmer.get(0));

        //workaround for Java
        List<Teilnehmer> teilnehmer_temp = new ArrayList<>();
        teilnehmer_temp.addAll(teilnehmer);

        spiel = new SpielStub(
                teilnehmer_temp,
                new ArrayList<>(),
                spielstand,
                "SpielStub"
        );

        spielpartie = new SpielpartieImpl(teilnehmer_temp,spielstand, spiel, stichstapel);
        //spielpartie = SpielpartieFactory.erstelleSpielpartie(teilnehmer, spielstand, spiel);
    }
    /* trivial
    public int getPartieNr() {
        return 0;
    }
    */

    /* trivial
    public Karte getTrumpf() {
        return null;
    }
    */

    /* zu testen:
        - ob die richtige Karten wurden den Teilnehmern übergegeben
        - Spiel darüber informiert wurde
        - ob ein Trumpf bei Stichstapel und Spielstand geset wurde
        - ob allen eine Möglichkeit von Drücken gegeben wurde
        - ob allen nacheinander die Möglichket Zug zu machen gegeben wurde
        - ob ein Stich richtig abgeschlossen wurde:
            - gewinner wurde richtig bestimmt
            - Spielstand wurde richtig geändert
        - ob alle SpielRObserver vom Stichende informiert wurden
        - ob Gewinnpunkte richtig vergeben wurden
        - ob alle SpielRObserver vom Ende der Spielpartie informiert wurden
        - ob die Spielpartie richtig reset wurde
     */
    @Test
    public void spielpartieDurchfueren() {

        spielpartie.spielpartieDurchfueren();
        for (TeilnehmerStub t : teilnehmer) {
            assertTrue("Teilnehmer muss 12 unterschiedliche Karten bekommen", t.obKartenVerteilt());
        }
        assertTrue("Spiel wurde wegen jeder verteiltetn Karte informiert.", spiel.obNotifiedAlleKartenVerteilt());
        assertTrue("Stichstapel muss ein Trumpf bekommen", stichstapel.obTrumpfSet());
        assertTrue("Spielstand muss ein Trumpf bekommen", spielstand.obTrumpfSet());
        assertTrue("Spiel muss informiert werden, dass Trumf set wurde", spiel.obNotified("notifyTrumpfSet"));
        assertTrue("Spiel muss von Drueckmoeglichkeit informiert werden", spiel.obNotified("notifyBereitZumDruecken"));
        assertTrue("Spiel wurde von Zugmöglichkeit für jeden Teilnehmer informiert", spiel.obNotifiedAlleSpielerAmZug() );
        assertTrue("Stich muss abgeschlossen werden", stichstapel.obAbgeschlossen() );
        assertTrue("Spiel muss richtig vom Stichende informiert werden", spiel.obNotified("notifyEndeStich") );
        //zu schwer für jetzt ;) assertTrue("Für jeden Teilnehmer, dem GP gegeben werden mussten, wurden sie gegeben", spielstand.obGPGegeben() );
        assertTrue("Spiel wurde von der Partieende notified", spiel.obNotified("notifyEndeSpielpartie") );
        assertTrue("Stichstapel muss reset werden", stichstapel.obReset() );

        //test that gewinnpunkteVerteilen takes care about Turnierregeln

    }


}
