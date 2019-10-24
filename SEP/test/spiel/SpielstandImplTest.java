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

/**
 * Created by rreimche on 12.06.15.
 */
public class SpielstandImplTest {
    Spielstand spielstand;
    List<Teilnehmer> teilnehmer = new ArrayList<>();

    @Before
    public void init() throws Exception{
        spielstand = new SpielstandImpl();
        spielstand.setTrumpf(new KarteImpl(Farbe.EIDEX, Symbol.KOENIG));
        for(int i=0;i<3;i++)
            try {
                teilnehmer.add(new Bot("Bot" + i));
            } catch (RemoteException e) {
                e.printStackTrace();
            }
    }
    /* trivial
    public int getGewinnpunkte(Teilnehmer t) {
        return 0;
    }
    */

    /* trivial
    public int getStichpunkte(Teilnehmer t) {
        return 0;
    }
    */

    @Test
    public void resetSpielpartie()  throws Exception{
        Karte karte = new KarteImpl(Farbe.EIDEX, Symbol.KOENIG);
        for(int i=0;i<3;i++) spielstand.zufuegeStichpunkte(teilnehmer.get(i), karte);
        spielstand.resetSpielpartie();
        for(int i=0;i<3;i++) assertEquals(spielstand.getStichpunkte(teilnehmer.get(i)), 0);
    }

    @Test
    public void zufuegeGewinnpunkte()  throws Exception{
        for(int i=0;i<3;i++){
            spielstand.zufuegeGewinnpunkte(teilnehmer.get(i), 3);
            assertEquals(spielstand.getGewinnpunkte(teilnehmer.get(i)), 3);
        }
    }

    @Test
    public void zufuegeStichpunkte() throws Exception {
        Karte karte = new KarteImpl(Farbe.EIDEX, Symbol.KOENIG);
        for(int i=0;i<3;i++){
            spielstand.zufuegeStichpunkte(teilnehmer.get(i), karte);
            assertEquals("Es musste korrekte Anzahl an Stichpunkte geben",spielstand.getStichpunkte(teilnehmer.get(i)), 4);
        }
    }
}
