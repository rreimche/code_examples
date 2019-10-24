package spiel;

import nutzer.Bot;
import nutzer.Teilnehmer;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.HashMap;
import java.util.Map;

/**
 * Implementationsklasse für Spielstand.
 */
public class SpielstandImpl extends UnicastRemoteObject implements Spielstand {
    private Karte trumpf;
    private final HashMap<Teilnehmer, Integer> gewinnpunkte = new HashMap<>();
    private final Map<Teilnehmer, Integer> stichpunkte = new HashMap<>();
    private final Map<Teilnehmer, Integer> drueckStichpunkte = new HashMap<>();

    public SpielstandImpl() throws RemoteException{

    }

    @Override
    public int getGewinnpunkte(Teilnehmer teilnehmer) throws RemoteException {
        if( gewinnpunkte.get(teilnehmer) == null  ) return 0;
        return gewinnpunkte.get(teilnehmer);
    }

    @Override
    public int getAlleStichpunkte() throws RemoteException {
        int sum = 0;
        for (Teilnehmer t : stichpunkte.keySet()){
            int a = stichpunkte.get(t) != null ? stichpunkte.get(t) : 0;
            sum+=a;
        }
        return sum;
    }

    @Override
    public int getDrueckStichpunkte(Teilnehmer teilnehmer) {
        if( drueckStichpunkte.get(teilnehmer) == null  ) return 0;
        return drueckStichpunkte.get(teilnehmer);
    }

    @Override
    public void zufuegeDrueckStichpunkte(Teilnehmer teilnehmer, Karte karte){
        if(drueckStichpunkte.get(teilnehmer) != null){
            drueckStichpunkte.put(teilnehmer, drueckStichpunkte.get(teilnehmer).intValue() + berechneStichpunkte(karte));
        }
        else {
            drueckStichpunkte.put(teilnehmer, berechneStichpunkte(karte));
        }
        /*
        if (drueckStichpunkte.containsKey(teilnehmer)){
            drueckStichpunkte.put(teilnehmer, drueckStichpunkte.get(teilnehmer).intValue() + berechneStichpunkte(karte));
        }
        else {
            drueckStichpunkte.put(teilnehmer, berechneStichpunkte(karte));
        }
        */
    }

    @Override
    public int getStichpunkte(Teilnehmer teilnehmer) {
        return stichpunkte.get(teilnehmer) != null ? stichpunkte.get(teilnehmer) : 0;
    }

    @Override
    public void resetSpielpartie() {
        stichpunkte.clear();
        drueckStichpunkte.clear();
    }

    @Override
    public void zufuegeGewinnpunkte(Teilnehmer teilnehmer, int punkte) {
        if(gewinnpunkte.get(teilnehmer) != null){
            gewinnpunkte.put(teilnehmer, gewinnpunkte.get(teilnehmer).intValue() + punkte);
        }
        else {
            gewinnpunkte.put(teilnehmer, punkte);
        }
        /*
        if (gewinnpunkte.containsKey(teilnehmer)){
            gewinnpunkte.put(teilnehmer, gewinnpunkte.get(teilnehmer).intValue() + punkte);
        }
        else {
            gewinnpunkte.put(teilnehmer, punkte);
        }
        */
    }

    @Override
    public void zufuegeStichpunkte(Teilnehmer teilnehmer, Karte karte) {
        if (stichpunkte.get(teilnehmer) != null){
            stichpunkte.put(teilnehmer, stichpunkte.get(teilnehmer).intValue()+berechneStichpunkte(karte));
        }
        else{
            stichpunkte.put(teilnehmer, +berechneStichpunkte(karte));
        }
        /*
        if( stichpunkte.containsKey(teilnehmer)){
            stichpunkte.put(teilnehmer, stichpunkte.get(teilnehmer).intValue()+berechneStichpunkte(karte));
        }
        else {
            stichpunkte.put(teilnehmer, +berechneStichpunkte(karte));
        }
        */
    }

    @Override
    public void zufuegeStichpunkteLetzterStich(Teilnehmer stichgewinner) {
        stichpunkte.put(stichgewinner, stichpunkte.get(stichgewinner).intValue()+ 5);
    }

    @Override
    public void setTrumpf(Karte k){ trumpf = k; }

    @Override
    public Karte getTrumpf() throws RemoteException {
        return trumpf;
    }

    private int berechneStichpunkte(Karte karte){
        //if the card is of Trumpf-Color (which implies that it is a normal game type, not Obenabe or Undenufe)
        if( karte.getFarbe() == trumpf.getFarbe() && trumpf.getSymbol() != Symbol.ASS && trumpf.getSymbol() != Symbol.SECHS )
        {
            switch ( karte.getSymbol() )
            {
                case BUBE:
                    return 20;
                case NEUN:
                    return 14;
                case ASS:
                    return 11;
                case KOENIG:
                    return 4;
                case DAME:
                    return 3;
                case ZEHN:
                    return 10;
                default:
                    return 0;

            }
        }
        //if the card is not of the trumpf color
        else
        {
            switch ( trumpf.getSymbol() )
            {
                //Obenabe Spielpartie
                case ASS:
                    switch (karte.getSymbol())
                    {
                        case ASS:
                            return 11;
                        case KOENIG:
                            return 4;
                        case DAME:
                            return 3;
                        case BUBE:
                            return 2;
                        case ZEHN:
                            return 10;
                        case ACHT:
                            return 8;
                        default:
                            return 0;
                    }
                //Undenufe Spielpartie
                case SECHS:
                    switch (karte.getSymbol())
                    {
                        case KOENIG:
                            return 4;
                        case DAME:
                            return 3;
                        case BUBE:
                            return 2;
                        case ZEHN:
                            return 10;
                        case ACHT:
                            return 8;
                        case SECHS:
                            return 11;
                        default:
                            return 0;
                    }
                //Normal Spielpartie
                default:
                    switch (karte.getSymbol())
                    {
                        case ASS:
                            return 11;
                        case KOENIG:
                            return 4;
                        case DAME:
                            return 3;
                        case BUBE:
                            return 2;
                        case ZEHN:
                            return 10;
                        default:
                            return 0;
                    }
            }
        }
    }

    @Override
    public void ersetzeTeilnehmer(Teilnehmer teilnehmer, Teilnehmer bot){
        Integer gp = gewinnpunkte.get(teilnehmer);
        gewinnpunkte.remove(teilnehmer);
        gewinnpunkte.put(bot, gp);

        Integer sp = stichpunkte.get(teilnehmer);
        stichpunkte.remove(teilnehmer);
        stichpunkte.put(bot, sp);

        Integer dsp = drueckStichpunkte.get(teilnehmer);
        drueckStichpunkte.remove(teilnehmer);
        drueckStichpunkte.put(bot, dsp);
    }
}
