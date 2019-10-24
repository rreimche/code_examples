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
public class KartenComparatorTest {
    KartenComparator comparator;
    Karte trumpf;

    ArrayList<Farbe> farben = new ArrayList<>();
    ArrayList<Symbol> symbole = new ArrayList<>();
    Random randomGenerator = new Random();

    @Before
    public void init(){

        for( Farbe f : Farbe.values() ) {
            farben.add(f);
        }

        for( Symbol s : Symbol.values() ) {
            symbole.add(s);
        }
    }

    @Test
    public void compareToGleich(){
        for( Farbe trumpfFarbe : Farbe.values() ){
            for( Symbol trumpfSymbol : Symbol.values() ){
                trumpf = new KarteImpl(trumpfFarbe, trumpfSymbol);
                comparator = new KartenComparator(trumpf);
                for( Farbe f : Farbe.values() ){
                    for( Symbol s : Symbol.values() ){
                        Karte k1 = new KarteImpl(f, s);
                        Karte k2 = new KarteImpl(f, s);
                        assertEquals( "Karten müssen gleichwertig erscheinen", 0, comparator.compare(k1, k2));
                    }
                }
            }
        }
    }

    @Test
    public void compareToUngleich(){
        for(Farbe trumpfFarbe : Farbe.values() ){
            for(Symbol trumpfSymbol : Symbol.values() ){
                trumpf = new KarteImpl(trumpfFarbe, trumpfSymbol);
                comparator = new KartenComparator(trumpf);

                for( Farbe f : Farbe.values() ){
                    for( Symbol s : Symbol.values() ){
                        Karte k1 = new KarteImpl(f, s);
                        Karte k2 = new KarteImpl(f, s);
                        while( k1.getSymbol() == k2.getSymbol() ){
                            int indexSymbole = randomGenerator.nextInt(symbole.size());
                            k2 = new KarteImpl( f, symbole.get(indexSymbole) );
                        }
                        String result = comparator.compare(k1, k2) == 0 ? "gleich" : "ungleich";
                        assertEquals( "Karten müssen ungleichwertig sein: " + k1.toString() + ", " + k2.toString(), "ungleich", result );
                    }
                }
            }
        }

    }

    @Test
    public void compareToHigher(){
        Symbol hoechste; //höchste Symbole für die vergleichende Farbe
        for( Farbe trumpfFarbe : Farbe.values() ) {
            for (Symbol trumpfSymbol : Symbol.values()) {
                trumpf = new KarteImpl(trumpfFarbe, trumpfSymbol);
                comparator = new KartenComparator(trumpf);

                for( Farbe f : Farbe.values() ){
                    for( Symbol s : Symbol.values() ){
                        Karte k1 = new KarteImpl(f, s);
                        switch (trumpfSymbol){
                            //if it's Obenabe (also ohne trumpf), das höchste Symbol ist ASS
                            case ASS:
                                hoechste = Symbol.ASS;
                                break;
                            //if it's Undenufe (also ohne trumpf), das höchste Symbol ist SECHS
                            case SECHS:
                                hoechste = Symbol.SECHS;
                                break;
                            //if it's a neither Undenufe, nor Obenabe (also mit trumpf), das höchste Symbol...
                            default:
                                if( trumpfFarbe == f ) { //...depends of trumpfFarbe == farbe
                                    hoechste = Symbol.BUBE;
                                } else {
                                    hoechste = Symbol.ASS;
                                }
                        }

                        if( s == hoechste ) continue; //es gibt kein Symbol höher als das Höchste
                        Karte k2 = new KarteImpl(f, hoechste);
                        assertTrue("Die 1. Karte muss weniger vom Wert sein, als die 2:" + k1.toString() + ", " + k2.toString(), comparator.compare(k1, k2) == -1);
                    }
                }
            }
        }


    }

    @Test
    public void compareToLower(){
        Symbol niedrigste;
        for( Farbe trumpfFarbe : Farbe.values() ){
            for( Symbol trumpfSymbol : Symbol.values() ){
                trumpf = new KarteImpl(trumpfFarbe, trumpfSymbol);
                comparator = new KartenComparator(trumpf);

                for( Farbe f : Farbe.values() ){
                    for( Symbol s : Symbol.values() ){
                        Karte k1 = new KarteImpl(f, s);
                        switch (trumpfSymbol){
                            //if it's Obenabe (also ohne trumpf), das niedrigste Symbol ist SECHS
                            case ASS:
                                niedrigste = Symbol.SECHS;
                                break;
                            //if it's Undenufe (also ohne trumpf), das niedrigste Symbol ist ASS
                            case SECHS:
                                niedrigste = Symbol.ASS;
                                break;
                            //if it's a neither Undenufe, nor Obenabe (also mit trumpf), das niedrigste Symbol ist SECHS (no difference what trumpf is)
                            default:
                                niedrigste = Symbol.SECHS;
                        }
                        if( s == niedrigste ) continue; //es gibt kein Symbol niedriger SECHS
                        Karte k2 = new KarteImpl(f, niedrigste);
                        assertTrue("Die 1. Karte muss mehr vom Wert sein, als die 2:" + k1.toString() + ", " + k2.toString(), comparator.compare(k1, k2) == 1);
                    }
                }
            }
        }
    }

    @Test
    public void compareToTrumpf(){
        for( Farbe trumpfFarbe : Farbe.values() ){
            for( Symbol trumpfSymbol : Symbol.values() ){
                trumpf = new KarteImpl(trumpfFarbe, trumpfSymbol);
                comparator = new KartenComparator(trumpf);

                //ohne trumpf (Untenufe, Obenabe) git es kein Trumpf => unmöglich zu vergleichen.
                if(trumpf.getSymbol() == Symbol.SECHS || trumpf.getSymbol() == Symbol.ASS) continue;

                for( Farbe f : Farbe.values() ){

                    //we dont need to compare trumpf cards, it is done in the test for equality
                    if( f.equals(trumpfFarbe) ) continue;

                    for( Symbol s : Symbol.values() ){
                        Karte k1 = new KarteImpl(f, s);
                        assertTrue("Die 1. Karte muss weniger vom Wert sein, als die 2. (Trumpf-) Karte:" + k1.toString() + ", " + trumpf.toString(),
                                comparator.compare(k1, trumpf) == -1);
                    }
                }
            }
        }
    }

    @Test
    public void compareUnvergleichbar(){

    }
}
