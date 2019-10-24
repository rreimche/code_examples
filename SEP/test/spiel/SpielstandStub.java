package spiel;

import nutzer.Teilnehmer;

import java.rmi.RemoteException;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by rreimche on 17.06.15.
 */
public class SpielstandStub implements Spielstand {
    HashMap<Teilnehmer, Integer> gewinnpunkte = new HashMap<>();
    private final Map<Teilnehmer, Integer> drueckStichpunkte = new HashMap<>();
    boolean trumpfset;

    public SpielstandStub(){}

    /*public SpielstandStub(HashMap<Teilnehmer, Integer> gp){
        gewinnpunkte.putAll(gp);
    }*/

    @Override
    public int getGewinnpunkte(Teilnehmer t) {
        if( gewinnpunkte.containsKey(t)) {
            return gewinnpunkte.get(t);
        } else {
            return 0;
        }
    }

    @Override
    public int getAlleStichpunkte() throws RemoteException {
        return 0;
    }

    @Override
    public int getDrueckStichpunkte(Teilnehmer teilnehmer) throws RemoteException {
        if( drueckStichpunkte.get(teilnehmer) == null  ) return 0;
        return drueckStichpunkte.get(teilnehmer);
    }

    @Override
    public void zufuegeDrueckStichpunkte(Teilnehmer teilnehmer, Karte karte) throws RemoteException {
        if(drueckStichpunkte.get(teilnehmer) != null){
            drueckStichpunkte.put(teilnehmer, drueckStichpunkte.get(teilnehmer).intValue() + 20);
        }
        else {
            drueckStichpunkte.put(teilnehmer, 20);
        }
    }

    @Override
    public int getStichpunkte(Teilnehmer t) {
        return 0;
    }

    @Override
    public void resetSpielpartie() {

    }

    @Override
    public void setTrumpf(Karte k) {
        trumpfset = true;
    }

    @Override
    public Karte getTrumpf() throws RemoteException {
        return new KarteImpl(Farbe.EIDEX,Symbol.NEUN);
    }

    @Override
    public void ersetzeTeilnehmer(Teilnehmer teilnehmer, Teilnehmer bot) {

    }

    @Override
    public void zufuegeGewinnpunkte(Teilnehmer t, int p) {
        if(gewinnpunkte.containsKey(t)) {
            gewinnpunkte.put(t, gewinnpunkte.get(t) + p);
        } else {
            gewinnpunkte.put(t, p);
        }
    }

    @Override
    public void zufuegeStichpunkte(Teilnehmer t, Karte k) {

    }

    @Override
    public void zufuegeStichpunkteLetzterStich(Teilnehmer stichgewinner) {

    }

    public boolean obTrumpfSet(){
        return trumpfset;
    }
}
