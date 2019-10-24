package spiel;

import nutzer.Teilnehmer;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by rreimche on 17.06.15.
 */
public class SpielpartieStub implements Spielpartie {
    Spielstand spielstand;
    List<Teilnehmer> teilnehmer = new ArrayList<>();
    boolean istDurchgefuert;

    public SpielpartieStub(List<Teilnehmer> t, Spielstand s){
        spielstand = s;
        teilnehmer.addAll(t);
    }

    /*@Override
    public int getPartieNr() {
        return 0;
    }*/

    @Override
    public Karte getTrumpf() {
        return null;
    }

    @Override
    public void spielpartieDurchfueren() {
        istDurchgefuert = true;

        /*
            stelle sicher, dass es genau ein Gewinner gibt
            (ohne Gewinner wird Spiel.spielpartieDurchführen() für immer laufen)
        */
        Teilnehmer gewinner;
        gewinner = teilnehmer.get(0);
       /* Iterator<Teilnehmer> iterator = new teilnehmer.iterator();
        while ( iterator.hasNext() ) {
            tn = iterator.next();
            if ( spielstand.getGewinnpunkte(tn) > spielstand.getGewinnpunkte(gewinner) ){
                gewinner = tn;
            }
            spielstand.zufuegeGewinnpunkte( gewinner, 7 - spielstand.getGewinnpunkte(gewinner) );
        }*/
        try{
            spielstand.zufuegeGewinnpunkte(gewinner, 7);
        } catch (RemoteException e) {
                e.printStackTrace();
        }

    }

    @Override
    public Teilnehmer getSpielerAnReihe() {
        return null;
    }

    @Override
    public void aktualisiereSpielerListe(String zuersetzenderTeilnehmer, Teilnehmer t2) throws RemoteException {

    }

    public boolean obDurchgefuehrt(){
        return istDurchgefuert;
    }

}
