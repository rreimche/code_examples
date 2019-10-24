package spiel;

import nutzer.Nutzer;
import nutzer.Teilnehmer;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by rreimche on 17.06.15.
 */
public class SpielStub implements Spiel, SpielRObservable {
    List<Teilnehmer> teilnehmer = new ArrayList<>();
    List<Nutzer> nutzer = new ArrayList<>();
    Spielstand spielstand;
    String name;
    Teilnehmer spielerAnReihe;
    private boolean isNotifiedBereitZumDruecken, isNotifiedEndeSpielpartie, isNotifiedEndeStich, isNotifiedKarteGelegt,
            isNotifiedKarteVerteilt, isNnotifiedSpielBereit, isNotifiedSpielerAmZug, isNotifiedTrumpfSet,
            isNotifiedSpielGebrochen, isNotifiedSpielBereit;
    int intNotifiedKarteVerteilt, intNotifiedSpielerAmZug;

    public SpielStub(){

    }

    public SpielStub(Teilnehmer spielerAnReihe){
        this.spielerAnReihe = spielerAnReihe;
    }

    public SpielStub(List<Teilnehmer> t, List<Nutzer> nr, Spielstand s, String n){

        teilnehmer.addAll(t);
        nutzer.addAll(nr);
        spielstand = s;
        name = n;
    }

    @Override
    public List<Teilnehmer> getTeilnehmerList() {
        return null;
    }

    @Override
    public void spielStarten(List<SpielRObserver> spielRObserverList) throws RemoteException {

    }

    @Override
    public void notifyBereitzumDruecken() {
        isNotifiedBereitZumDruecken = true;
    }

    @Override
    public void notifyLosGehts() throws RemoteException {

    }

    @Override
    public void notifyShowKarteGedruecktAndere(String spielername, Karte karte) throws RemoteException {

    }

    @Override
    public void notifyShowKarteGelegt(String spielername, Karte karte) throws RemoteException {

    }

    @Override
    public void notifyEndeSpielpartie(String teilnehmer0, int gewinnpunkteTeilnehmer0, String teilnehmer1, int gewinnpunkteTeilnehmer1, String teilnehmer2, int gewinnpunkteTeilnehmer2) {
        isNotifiedEndeSpielpartie = true;
    }

    @Override
    public void notifyEndeStich(String teilnehmer,List<String> teilnehmerStichpunkteList) {
        isNotifiedEndeStich = true;
    }

    @Override
    public void notifyKarteGelegt(String teilnehmer, Karte karte) {
        isNotifiedKarteGelegt = true;
    }

    @Override
    public void notifyKarteVerteilt(Karte karte, String name) {
        isNotifiedKarteVerteilt = true;
        intNotifiedKarteVerteilt++;
    }

    @Override
    public void notifySpielBereit(boolean bereit) {
        isNotifiedSpielBereit = true;
    }

    @Override
    public void notifySpielerAmZug(Teilnehmer teilnehmer) {
        isNotifiedSpielerAmZug = true;
        intNotifiedSpielerAmZug++;
    }

    @Override
    public void notifyTrumpfSet(Karte karte) {
        isNotifiedTrumpfSet = true;
    }

    @Override
    public void notifySpielGebrochen(Teilnehmer teilnehmer) {
        isNotifiedSpielGebrochen = true;
    }

    @Override
    public Spielstand getSpielstand() {
        return null;
    }

    @Override
    public Teilnehmer getSpielerAnReihe() throws RemoteException {
        return spielerAnReihe;
    }

    @Override
    public void countDownLege() throws RemoteException {

    }

    @Override
    public void countDownDruecke() throws RemoteException {

    }

    @Override
    public void ersetzeSpieler(String spielerName, Spiel spiel) throws RemoteException {

    }


    /*@Override
    public Teilnehmer getSpielerAnReihe() throws RemoteException {
        return null;
    }*/

    @Override
    public void addObserver(SpielRObserver ro) {
    }

    @Override
    public void removeObserver(SpielRObserver ro) {
    }

    @Override
    public void removeObserver(String name) throws RemoteException {

    }

    public boolean obNotified(String functionsname){
        switch(functionsname) {
            case "notifyBereitZumDruecken":
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
            case "notifySpielGebrochen":
                return isNotifiedSpielGebrochen;
            default:
                return false;
        }

    }

    public boolean obNotifiedAlleKartenVerteilt(){
        if( intNotifiedKarteVerteilt == 36 ) return true;
        return false;
    }

    public boolean obNotifiedAlleSpielerAmZug(){
        if( intNotifiedSpielerAmZug >= 3 ) return true;
        return false;
    }
}
