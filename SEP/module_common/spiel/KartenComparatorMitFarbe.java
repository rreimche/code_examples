package spiel;

/**
 * Der Comparator, der vom Blatt benutzt wird, um Karten zu sortieren. Unterscheidet sich vom KartenKomparator,
 * indem er keinen Trumpf berücksichtigt.
 * @see KartenComparator
 */
public class KartenComparatorMitFarbe extends KartenComparator{
    public KartenComparatorMitFarbe() {
        super(new KarteImpl(Farbe.EIDEX, Symbol.ACHT)); //Nur deshalb, weil die Eltern-Klasse das braucht.
    } //Braucht kein Trumpf

    @Override
    public int compare(Karte k1, Karte k2) {
        return compareNormal(k1, k2);
    }

    @Override
    protected int compareNormal(Karte k1, Karte k2){
        karten[0] = k1;
        karten[1] = k2;

        vergleichFarben = compareFarben(k1, k2);
        if( vergleichFarben != 0 ){
            return vergleichFarben;
        }
        assignWeightsNormal(0);
        return compareWeights();
    }

    @Override
    protected int compareObenabe(Karte k1, Karte k2){
        return compareNormal(k1, k2);
    }

    @Override
    protected int compareUndenufe(Karte k1, Karte k2){
        assignWeightsUndenufe();
        return compareWeights();
    }


    /* if Farben are equal return 0
        if Farben are not equal, return 2
     */
    @Override
    protected int compareFarben(Karte k1, Karte k2){

        int[] weights = new int[2];
        Karte[] karten = new Karte[2];
        karten[0] = k1;
        karten[1] = k2;
        for(int i = 0; i<2; i++){
            switch (karten[i].getFarbe()){
                case HERZ:
                    weights[i] = 3;
                    break;
                case EIDEX:
                    weights[i] = 2;
                    break;
                case RABE:
                    weights[i] = 1;
                    break;
                case STERN:
                    weights[i] = 0;
                    break;
            }
        }

        if( weights[0] < weights[1] ){
            return -1;
        } else if ( weights[0] == weights[1] ){
            return 0;
        } else {
            return 1;
        }
    }
}
