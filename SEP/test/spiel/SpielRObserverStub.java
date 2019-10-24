package spiel;

import nutzer.Teilnehmer;

import java.rmi.RemoteException;
import java.util.List;

/**
 * Created by rreimche on 17.06.15.
 */
public class SpielRObserverStub implements SpielRObserver {

    private boolean isNotifiedBereitZumDruecken, isNotifiedEndeSpielpartie, isNotifiedEndeStich, isNotifiedKarteGelegt,
            isNotifiedKarteVerteilt, isNnotifiedSpielBereit, isNotifiedSpielerAmZug, isNotifiedTrumpfSet,
            isNotifiedSpielBeendet, isNotifiedSpielAngefangen, isNotifiedSpielGebrochen, isNotifiedSpielBereit,
            isNotifiedLosGehts, isNotifiedShowKarteGedruecktAndere;

    @Override
    public void notifyBereitZumDruecken(SpielRObservable spiel) {
        isNotifiedBereitZumDruecken = true;
        try {
            ((Spiel) spiel).countDownDruecke();
        } catch (RemoteException e){
            e.printStackTrace();
        }
    }

    @Override
    public void notifyLosGehts(SpielRObservable spiel) throws RemoteException {
        isNotifiedLosGehts = true;
        ((Spiel) spiel).countDownDruecke();
    }

    @Override
    public void notifyShowKarteGedruecktAndere(String spielername, Karte karte) throws RemoteException {
        isNotifiedShowKarteGedruecktAndere = true;
    }

    @Override
    public void notifySpielAngefangen(SpielRObservable sielRObseravble, List<Teilnehmer> teilnehmerList) throws RemoteException {
        isNotifiedSpielAngefangen = true;
    }

    @Override
    public void notifyKarteGelegtAndere(String spielername, Karte karte) {

    }

    @Override
    public void notifyEndeSpielpartie(SpielRObservable spielRObservable, String teilnehmer0, int gewinnpunkteTeilnehmer0, String teilnehmer1, int gewinnpunkteTeilnehmer1, String teilnehmer2, int gewinnpunkteTeilnehmer2) { isNotifiedEndeSpielpartie = true; }

    @Override
    public void notifyEndeStich(SpielRObservable spielRObservable, String teilnehmer,List<String> teilnehmerStichpunkteList) { isNotifiedEndeStich = true;  }

    @Override
    public void notifyKarteGelegt(SpielRObservable spielRObservable, String teilnehmer, Karte karte) { isNotifiedKarteGelegt = true; }

    @Override
    public void notifyKarteVerteilt(SpielRObservable spielRObservable, Karte karte, String name) { isNotifiedKarteVerteilt = true; }

    @Override
    public void notifySpielBereit(SpielRObservable spielRObservable, boolean b) { isNotifiedSpielBereit = true; }

    @Override
    public void notifySpielerAmZug(SpielRObservable spiel, String teilnehmer) {
        isNotifiedSpielerAmZug = true;
        try {
            ((Spiel) spiel).countDownLege();
        } catch (RemoteException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void ping(String nachricht) throws RemoteException{

    }

    @Override
    public void notifyTrumpfSet(SpielRObservable spielRObservable, Karte karte) { isNotifiedTrumpfSet = true; }

    @Override
    public void notifySpielBeendet(String teilnehmer1, int punkteTeilnehmer1, String teilnehmer2, int punkteTeilnehmer2, String teilnehmer3, int punkteTeilnehmer3) throws RemoteException {
        isNotifiedSpielBeendet = true;
    }

    /*@Override
    public void notifySpielBeendet() throws RemoteException {

    }*/

    @Override
    public void notifySpielGebrochen(Teilnehmer t) {
        isNotifiedSpielGebrochen = true;
    }

    @Override
    public void notifyChangeLabel(String alterName, String neuerName) throws RemoteException {

    }

    @Override
    public String getIdentifier() throws RemoteException {
        return null;
    }

    /*
    public boolean obSpielBeendet(){
        return isNotifiedSpielBeendet;
    }


    public boolean obSpielGebrochen(){ return isNotifiedSpielGebrochen; }*/

    public boolean obNotified(String functionsname){
        switch(functionsname) {
            case "notifyBereitzumDruecken":
                return isNotifiedBereitZumDruecken;
            case "notifyEndeSpielpartie":
                return isNotifiedEndeSpielpartie;
            case "notifyEndeStich":
                return isNotifiedEndeStich;
            case "notifyKarteGelegt":
                return isNotifiedKarteGelegt;
            case "notifyKarteVerteilt":
                return isNotifiedKarteVerteilt;
            case "notifySpielBereit":
                return isNotifiedSpielBereit;
            case "notifySpielerAmZug":
                return isNotifiedSpielerAmZug;
            case "notifyTrumpfSet":
                return isNotifiedTrumpfSet;
            case "notifySpielBeendet":
                return isNotifiedSpielBeendet;
            case "notifySpielGebrochen":
                return isNotifiedSpielGebrochen;
            case "notifySpielAngefangen":
                return isNotifiedSpielAngefangen;
            case "notifyLosGehts":
                return isNotifiedLosGehts;
            case "notifyShowKarteGedruecktAndere":
                return isNotifiedShowKarteGedruecktAndere;
            default:
                return false;
        }

    }

}
