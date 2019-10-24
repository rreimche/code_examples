package spiel;

public class KarteImpl implements Karte {
    private final Farbe farbe;
    private final Symbol symbol;

    public KarteImpl(Farbe f, Symbol s){
        farbe = f;
        symbol = s;
    }

    @Override
    public Farbe getFarbe() {
        return farbe;
    }

    @Override
    public Symbol getSymbol() {
        return symbol;
    }

    @Override
    public String toString(){
        return farbe.toString() + " " + symbol.toString();
    }

    @Override
    public boolean equals(Object o){
        if( o.getClass() != this.getClass() ) return false;
        return isEqual((Karte)o);
    }

    private boolean isEqual(Karte k){
        if(k.getFarbe() == this.getFarbe() && k.getSymbol() == this.getSymbol() ) return true;
        return false;
    }

}
