package spiel;

import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Random;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * Created by rreimche on 05.07.15.
 */
public class KartenComparatorMitFarbeTest {
    KartenComparatorMitFarbe comparator;


    ArrayList<Farbe> farben = new ArrayList<>();
    ArrayList<Symbol> symbole = new ArrayList<>();
    Random randomGenerator = new Random();

    @Before
    public void init(){
        comparator = new KartenComparatorMitFarbe();

        for( Farbe f : Farbe.values() ) {
            farben.add(f);
        }

        for( Symbol s : Symbol.values() ) {
            symbole.add(s);
        }
    }

    @Test
    public void compareToGleich(){
        for( Farbe f : Farbe.values() ){
            for( Symbol s : Symbol.values() ){
                Karte k1 = new KarteImpl(f, s);
                Karte k2 = new KarteImpl(f, s);
                assertEquals( "Karten müssen gleichwertig erscheinen", 0, comparator.compare(k1, k2));
            }
        }
    }

    @Test
    public void compareToUngleich(){
        for( Farbe f : Farbe.values() ){
            for( Symbol s : Symbol.values() ){
                Karte k1 = new KarteImpl(f, s);
                Karte k2 = new KarteImpl(f, s);
                while( k1.getFarbe() == k2.getFarbe() && k1.getSymbol() == k2.getSymbol() ){
                    int indexFarben = randomGenerator.nextInt(farben.size());
                    int indexSymbole = randomGenerator.nextInt(symbole.size());
                    k2 = new KarteImpl( farben.get(indexFarben), symbole.get(indexSymbole) );
                }
                String result = comparator.compare(k1, k2) == 0 ? "gleich" : "ungleich";
                assertEquals( "Karten müssen ungleichwertig sein", "ungleich", result );
            }
        }
    }

    @Test
    public void compareToHigher(){
        for( Farbe f : Farbe.values() ){
            for( Symbol s : Symbol.values() ){
                Karte k1 = new KarteImpl(f, s);
                if( s == Symbol.ASS ) continue; //es gibt kein Symbol höher ASS, wenn's kein Trumpf gibt
                Karte k2 = new KarteImpl(f, Symbol.ASS);
                assertTrue("Die Karte muss weniger vom Wert sein, als die 2:" + k1.toString() + ", " + k2.toString(), comparator.compare(k1, k2) == -1);
            }
        }
    }

    @Test
    public void compareToLower(){
        for( Farbe f : Farbe.values() ){
            for( Symbol s : Symbol.values() ){
                Karte k1 = new KarteImpl(f, s);
                if( s == Symbol.SECHS ) continue; //es gibt kein Symbol niedriger SECHS, wenn's kein Trumpf gibt
                Karte k2 = new KarteImpl(f, Symbol.SECHS);
                assertTrue("Die 1. Karte muss mehr vom Wert sein, als die 2:" + k1.toString() + ", " + k2.toString(), comparator.compare(k1, k2) == 1);
            }
        }
    }

    @Test
    public void compareToLowerFarbe(){
        Farbe f1 = Farbe.HERZ;
        for( Farbe f2 : Farbe.values()){
            if( f1.equals(f2) ) continue; //no need to compare two equal colors
            int indexSymbole = randomGenerator.nextInt(symbole.size());
            Karte k1 = new KarteImpl(f1, symbole.get(indexSymbole));
            Karte k2 = new KarteImpl(f2, symbole.get(indexSymbole));
            assertTrue("Die HERZ-Karte muss mehrwertig sein als die 2.:" + k1.toString() + " " + k2.toString(), comparator.compare(k1, k2) == 1);
        }

        f1 = Farbe.EIDEX;
        for( Farbe f2 : Farbe.values()){
            if( f2.equals(Farbe.HERZ) ) continue; //no need to compare to higher color
            if( f1.equals(f2) ) continue; //no need to compare two equal colors
            int indexSymbole = randomGenerator.nextInt(symbole.size());
            Karte k1 = new KarteImpl(f1, symbole.get(indexSymbole));
            Karte k2 = new KarteImpl(f2, symbole.get(indexSymbole));
            assertTrue("Die EIDEX-Karte muss mehrwertig sein als die 2.:" + k1.toString() + " " + k2.toString(), comparator.compare(k1, k2) == 1);
        }

        f1 = Farbe.RABE;
        for( Farbe f2 : Farbe.values()){
            if( f2.equals(Farbe.HERZ) || f2.equals(Farbe.EIDEX) ) continue; //no need to compare to higher color
            if( f1.equals(f2) ) continue; //no need to compare two equal colors
            int indexSymbole = randomGenerator.nextInt(symbole.size());
            Karte k1 = new KarteImpl(f1, symbole.get(indexSymbole));
            Karte k2 = new KarteImpl(f2, symbole.get(indexSymbole));
            assertTrue("Die RABE-Karte muss mehrwertig sein als die 2.:" + k1.toString() + " " + k2.toString(), comparator.compare(k1, k2) == 1);
        }

    }

    @Test
    public void compareToHigherFarbe(){
        Farbe f1 = Farbe.STERN;
        for( Farbe f2 : Farbe.values()){
            if( f1.equals(f2) ) continue; //no need to compare two equal colors
            int indexSymbole = randomGenerator.nextInt(symbole.size());
            Karte k1 = new KarteImpl(f1, symbole.get(indexSymbole));
            Karte k2 = new KarteImpl(f2, symbole.get(indexSymbole));
            assertTrue("Die STERN-Karte muss wenigerwertig sein als die 2.:" + k1.toString() + " " + k2.toString(), comparator.compare(k1, k2) == -1);
        }

        f1 = Farbe.RABE;
        for( Farbe f2 : Farbe.values()){
            if( f2.equals(Farbe.STERN) ) continue; //no need to compare to lower color
            if( f1.equals(f2) ) continue; //no need to compare two equal colors
            int indexSymbole = randomGenerator.nextInt(symbole.size());
            Karte k1 = new KarteImpl(f1, symbole.get(indexSymbole));
            Karte k2 = new KarteImpl(f2, symbole.get(indexSymbole));
            assertTrue("Die RABE-Karte muss wenigerwertig sein als die 2.:" + k1.toString() + " " + k2.toString(), comparator.compare(k1, k2) == -1);
        }

        f1 = Farbe.EIDEX;
        for( Farbe f2 : Farbe.values()){
            if( f2.equals(Farbe.STERN) || f2.equals(Farbe.RABE) ) continue; //no need to compare to higher color
            if( f1.equals(f2) ) continue; //no need to compare two equal colors
            int indexSymbole = randomGenerator.nextInt(symbole.size());
            Karte k1 = new KarteImpl(f1, symbole.get(indexSymbole));
            Karte k2 = new KarteImpl(f2, symbole.get(indexSymbole));
            assertTrue("Die EIDEX-Karte muss wenigerwertig sein als die 2.:" + k1.toString() + " " + k2.toString(), comparator.compare(k1, k2) == -1);
        }

    }
}
