package spiel;

import exceptions.KartenUnvergleichbarException;

import java.io.Serializable;
import java.util.Comparator;
import java.util.Random;

/**
 * Der Comparator, der vom Stichstapel benutzt wird, um Karten miteinander zu vergleichen.
 */
public class KartenComparator implements Comparator<Karte>, Serializable {

    protected Karte trumpf;

    protected int vergleichFarben,weightK1, weightK2;
    protected boolean istUnnormalePartie; //ob die Partie Obenobe oder Untenufe ist

    protected Karte[] karten = new Karte[2]; //an array to go through when assigning weights
    protected int[] weightKarte = new int[2]; //an array to keep weights for k1 and k2 respectfully
    protected int i; // just an iterator variable for multiple purposes

    protected static Random rand = new Random();


    public KartenComparator(Karte t){
        trumpf = t;
    }

    /**
     * Für diese Methode gibt ein eine Differenz davon, was für die Classe Comparator&lt;T&gt; gilt:
     * wenn 2 Karten von unterschiedlichen Farben sind und keine davon Trumpffarbe ist,
     * gibt die Methode eine zufällige ganze Zahl weniger -1 zurück, was bedeutet, dass Karten unvergleichbar in dieser Spielpartie
     * sind.
     * @see Comparator
     * @param k1
     * @param k2
     * @return  -1, wenn erste Karte wenigerwertig als die 2. ist, +1 wenn umgekehrt und 0 wenn sie VOM WERT gleich sind
     *          Eine beliebige Zahl weniger -1, wenn die Karte unvergleichbar sind.
     */
    @Override
    public int compare(Karte k1, Karte k2) {
        //determine the game type and compare accordingly
        try {
            switch (trumpf.getSymbol()) {
                case ASS:
                    istUnnormalePartie = true;
                    return compareObenabe(k1, k2);
                case SECHS:
                    istUnnormalePartie = true;
                    return compareUndenufe(k1, k2);
                default:
                    return compareNormal(k1, k2);
            }
        }catch (KartenUnvergleichbarException e){
            //e.printStackTrace();
            return randInt(-2147483648, -2); //bedeutet "Karten für diese Partei sind unvergleichbar"
        }
    }

    protected int compareNormal(Karte k1, Karte k2) throws KartenUnvergleichbarException{
        karten[0] = k1;
        karten[1] = k2;

        vergleichFarben = compareFarben(k1, k2);

        //if one is trumpf and the other is not, one is always higher
        if(vergleichFarben == -1 || vergleichFarben == 1) return vergleichFarben;

        assignWeightsNormal(vergleichFarben);

        return compareWeights();

    }

    protected int compareObenabe(Karte k1, Karte k2) throws KartenUnvergleichbarException{ //no trumpf
        karten[0] = k1;
        karten[1] = k2;

        compareFarben(k1, k2); //either throws an exception or makes sure we can compare these cards

        i = 0;

        for( Karte k : karten){
            switch (k.getSymbol()){
                case ASS:
                    weightKarte[i] = 8;
                    break;
                case KOENIG:
                    weightKarte[i] = 7;
                    break;
                case DAME:
                    weightKarte[i] = 6;
                    break;
                case BUBE:
                    weightKarte[i] = 5;
                    break;
                case ZEHN:
                    weightKarte[i] = 4;
                    break;
                case NEUN:
                    weightKarte[i] = 3;
                    break;
                case ACHT:
                    weightKarte[i] = 2;
                    break;
                case SIEBEN:
                    weightKarte[i] = 1;
                    break;
                case SECHS:
                    weightKarte[i] = 0;
                    break;
                default:
            }
            i++;
        }

        return compareWeights();

    }

    protected int compareUndenufe(Karte k1, Karte k2) throws KartenUnvergleichbarException{
        karten[0] = k1;
        karten[1] = k2;

       // try{
            compareFarben(k1, k2); //either throws an exception or makes sure we can compare these cards
        //} catch ( KartenUnvergleichbarException e){}

        assignWeightsUndenufe();

        return compareWeights();

    }

    /* if Farben are equal and not Trumpf, return 0
        if Farben are equal and Trumpf, return 2
        if Farben are different and one is Trumpf, then it is higher ordered (return -1 or 1)
        if Farben are different and none is Trumpf, throw KartenUnvergleichbarException (we cannot compare in this case)

     */
    protected int compareFarben(Karte k1, Karte k2) throws KartenUnvergleichbarException{
        // equal non-trump colors
        if( k1.getFarbe() == k2.getFarbe() && ( k2.getFarbe() != trumpf.getFarbe() || istUnnormalePartie ) ){
            return 0;
        // equal trump colors
        }else if(k1.getFarbe() == k2.getFarbe()) {
            return 2;
        // first is trumpf, the second is not
        }else if(k1.getFarbe() == trumpf.getFarbe()) {
            if(istUnnormalePartie) throw new KartenUnvergleichbarException();
            return 1;
        // first is non-trump, but the second one is
        }else if(k2.getFarbe() == trumpf.getFarbe()) {
            if(istUnnormalePartie) throw new KartenUnvergleichbarException();
            return -1;
        // cards are of different colors and no one of them is trumpf
        }else{
            throw new KartenUnvergleichbarException();
        }
    }

    protected void assignWeightsNormal(int vergleichFarben){
        //if both are trumpf, we have one logic for compare process, if both are not trumpf, the other
        if(vergleichFarben == 2){

            i = 0;

            for(Karte k : karten){

                switch (k.getSymbol()){
                    case BUBE:
                        weightKarte[i] = 8;
                        break;
                    case NEUN:
                        weightKarte[i] = 7;
                        break;
                    case ASS:
                        weightKarte[i] = 6;
                        break;
                    case KOENIG:
                        weightKarte[i] = 5;
                        break;
                    case DAME:
                        weightKarte[i] = 4;
                        break;
                    case ZEHN:
                        weightKarte[i] = 3;
                        break;
                    case ACHT:
                        weightKarte[i] = 2;
                        break;
                    case SIEBEN:
                        weightKarte[i] = 1;
                        break;
                    case SECHS:
                        weightKarte[i] = 0;
                        break;
                    default:
                }

                i++;
            }
            //if both are trumpf, we have one logic for compare process, if both are not trumpf, the other
        }else if(vergleichFarben == 0){

            i = 0;

            for( Karte k : karten) {
                switch (k.getSymbol()) {
                    case ASS:
                        weightKarte[i] = 8;
                        break;
                    case KOENIG:
                        weightKarte[i] = 7;
                        break;
                    case DAME:
                        weightKarte[i] = 6;
                        break;
                    case BUBE:
                        weightKarte[i] = 5;
                        break;
                    case ZEHN:
                        weightKarte[i] = 4;
                        break;
                    case NEUN:
                        weightKarte[i] = 3;
                        break;
                    case ACHT:
                        weightKarte[i] = 2;
                        break;
                    case SIEBEN:
                        weightKarte[i] = 1;
                        break;
                    case SECHS:
                        weightKarte[i] = 0;
                        break;
                }

                i++;
            }
        }
    }

    protected void assignWeightsUndenufe(){
        i = 0;

        for( Karte k : karten){
            switch (k.getSymbol()){
                case ASS:
                    weightKarte[i] = 0;
                    break;
                case KOENIG:
                    weightKarte[i] = 1;
                    break;
                case DAME:
                    weightKarte[i] = 2;
                    break;
                case BUBE:
                    weightKarte[i] = 3;
                    break;
                case ZEHN:
                    weightKarte[i] = 4;
                    break;
                case NEUN:
                    weightKarte[i] = 5;
                    break;
                case ACHT:
                    weightKarte[i] = 6;
                    break;
                case SIEBEN:
                    weightKarte[i] = 7;
                    break;
                case SECHS:
                    weightKarte[i] = 8;
                    break;
                default:
            }
            i++;
        }
    }

    protected int compareWeights() {
        if (weightKarte[0] > weightKarte[1]) return 1;
        if (weightKarte[0] < weightKarte[1]) return -1;
        return 0; //though, it is impossible for 2 players to have 2 equal cards, it's simplier to not throw an exception here ,)
    }

    /**
     * Gibt eine zufällige ganze Zahl zwischen min und max inklusive.
     * Die Differenz zwischen min und max kann maximal sein:
     * <code>Integer.MAX_VALUE - 1</code>.
     *
     * @param min Minimum
     * @param max Maximum. Muss mehr als min sein.
     * @return Ganze zahl zwischen min und max inklusive.
     * @see java.util.Random#nextInt(int)
     */

    protected static int randInt(int min, int max) {

        // nextInt is normally exclusive of the top value,
        // so add 1 to make it inclusive
        int randomNum = rand.nextInt((max - min) + 1) + min;

        return randomNum;
    }

}
