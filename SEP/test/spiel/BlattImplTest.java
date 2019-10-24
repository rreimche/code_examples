package spiel;

import exceptions.KarteSchonVorhandenException;
import org.junit.* ;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Random;
import java.util.SortedSet;
import java.util.concurrent.ConcurrentSkipListSet;

import static org.junit.Assert.* ;

/**
 * Created by rreimche on 12.06.15.
 */
public class BlattImplTest {
    Blatt blatt;
    ConcurrentSkipListSet blattmuster;
    //Karte trumpf;
    KartenComparator comparator;

    @Before
    public void init() {
        //trumpf = new KarteImpl(Farbe.HERZ, Symbol.SIEBEN);
        comparator = new KartenComparatorMitFarbe();
        blatt = new BlattImpl(comparator);
        blattmuster = new ConcurrentSkipListSet(comparator);
        ArrayList<Farbe> farben = new ArrayList<>();
        ArrayList<Symbol> symbole = new ArrayList<>();
        Random randomGenerator = new Random();
        for( Farbe f : Farbe.values() ) {
            farben.add(f);
        }

        for( Symbol s : Symbol.values() ) {
            symbole.add(s);
        }

        while(blatt.getBlatt().size() < 12) {
            int indexFarben = randomGenerator.nextInt(farben.size());
            int indexSymbole = randomGenerator.nextInt(symbole.size());
            KarteImpl karte = new KarteImpl( farben.get(indexFarben), symbole.get(indexSymbole) );

            try{
                blatt.karteZufuegen(karte);
            } catch( KarteSchonVorhandenException e){ System.out.println("Karte schon vorhanden, aber egal"); }

        }


        for(Karte k : blatt.getBlatt()){
            boolean test = blattmuster.contains(k);
            blattmuster.add(k);
        }
    }
    
    /*
    * Karten des Blattes können in ConcurrentSkipListSet<Karte> sein
    */

    /*
    * trivial
    
    public void getBlatt() {
        return null;
    }*/
    
    /* trivial
    public void karteEntfernen(Karte k) {
        return null;
    }
    */

    /* trivial
    public void karteZufuegen(Karte k) {

    }
    */
    
    /*
    * erstelle unsortiertes Blatt, kopiere und zuweise, sortiere, getBlatt, vergleiche mit erstem
    */
    @Test
    public void sortiereBlattFarblichAufsteig() {
        blatt.sortiereBlattFarblichAufsteig();
        ArrayList<Karte> dasblatt = blatt.getBlatt();
        /* beide blätter haben gleiche Karten und mit gleicher Sortierung muss dann jedes Paar von karten von gleichwertigen
            Karten entstehen.
         */
        assertArrayEquals("The cards must be equal", blattmuster.toArray(), dasblatt.toArray());
    }
    
    /*
    * erstelle unsortiertes Blatt, kopiere und zuweise, sortiere, getBlatt, vergleiche mit erstem
    */
    @Test
    public void sortiereBlattFarblichAbsteig() {
        blatt.sortiereBlattFarblichAbsteig();
        ArrayList<Karte> dasblatt = blatt.getBlatt();
        /* beide blätter haben gleiche Karten und mit gleicher Sortierung muss dann jedes Paar von karten von gleichwertigen
            Karten entstehen.
         */
        assertArrayEquals("The cards must be equal", blattmuster.descendingSet().toArray(), dasblatt.toArray());
    }
}
