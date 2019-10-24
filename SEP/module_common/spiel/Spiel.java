package spiel;

import nutzer.Teilnehmer;

import java.rmi.RemoteException;
import java.util.List;

/**
 * Ein gesamtes spiel. Dient der Verwaltung des Spieles.
 */
public interface Spiel extends SpielRObservable{
    /**
     * Liefert eine Liste aller Teilnehmer zurück, die am Spiel teilnehmen
     * @return      Liste aller Teilnehmer, die an diesem Spiel teilnehmen
     */
    List<Teilnehmer> getTeilnehmerList() throws RemoteException;

    /**
     * Startet das Spiel und lässt Spielpartien durchführen, solange noch kein Spielgewinner festgelegt wurde
     * @param spielRObserverList Liste der Observer, die benachrichtigt werden sollen
     * @throws RemoteException
     */
    void spielStarten(List<SpielRObserver> spielRObserverList) throws RemoteException;

    /**
     * Benachrichtigt den SpielObserver, dass nun die Karten gedrückt werden sollen.
     * @throws RemoteException
     */
    void notifyBereitzumDruecken() throws RemoteException;

    /**
     *
     * @throws RemoteException
     */
    void notifyLosGehts() throws RemoteException;

    /**
     * Benachrichtigt alle Observer des Spiels, dass der Spieler mit dem als Parameter übergebenen Namen eine Karte gedrückt
     * hat
     * @param spielername           Name des Spielers, der die Karte gedrückt hat
     * @param karte                 Karte, die der Spieler gedrückt hat
     * @throws RemoteException
     */
    void notifyShowKarteGedruecktAndere(String spielername, Karte karte) throws RemoteException;

    /**
     * Benachrichtigt die Observer, dass ein Spieler eine Karte gelegt hat, und übergibt dazu als Parameter die Angaben
     * Diese Funktion wird aufgerufen, um zu signalisieren, dass die bei den anderen Clienten anzeigt wird,
     * wer eine Karte gedrückt hat
     * @param spielername           Name des Spielers, der die Karte gelegt hat
     * @param karte                 Karte die der Spieler gelegt hat
     * @throws RemoteException
     */
    void notifyShowKarteGelegt(String spielername, Karte karte) throws RemoteException;

    /**
     * Teilt den SpielRObservern mit, dass eine Karte gelegt wurde.
     * @param teilnehmer        Teilnehmer, der die Karte gelegt hat
     * @param karte             Die entsprechend gelegte Karte
     */
    void notifyKarteGelegt(String teilnehmer, Karte karte) throws RemoteException;

    /**
     * Teilt den SpielRObservern mit, dass die aktuelle Spielpartie zu Ende ist.
     */
    void notifyEndeSpielpartie(String teilnehmer0, int gewinnpunkteTeilnehmer0, String teilnehmer1, int gewinnpunkteTeilnehmer1, String teilnehmer2, int gewinnpunkteTeilnehmer2) throws RemoteException;

    /**
     * Teilt den SpielRObservern mit, dass der aktuelle Stich zu Ende ist und übermittelt den Teilnehmer, der
     * gewonnen hat
     * @param teilnehmer        Teilnehmer, der den Stich gewonnen hat
     */
    void notifyEndeStich(String teilnehmer, List<String> teilnehmerStichpunkteList) throws RemoteException;


    /**
     * Teilt den SpielRObservern mit, welche Karte der Spieler bekommen hat
     * @param karte             Karte, die der Spieler im Zuge des Kartenverteilens bekommt
     * @param name              Name des teilnehmers
     */
    void notifyKarteVerteilt(Karte karte, String name) throws RemoteException;

    /**
     * Teilt den SpielRObservern des Anfuehrers mit, dass alle anderen Spieler bereit sind
     * @param bereit            Wert, der angibt, ob alle anderen Spieler bereit sind, sodass das Spiel dann gestartet
     *                          werden kann
     */
    void notifySpielBereit(boolean bereit) throws RemoteException;

    /**
     * Teilt den SpielRObservern mit, das der als Parameter uebergebene Teilnehmer nun am Zug ist
     * @param teilnehmer        Teilnehmer der am Zug ist
     */
    void notifySpielerAmZug(Teilnehmer teilnehmer) throws RemoteException;

    /**
     * Teilt den SpielRObservern mit, welche Karte jetzt als Trumpf bestimmt ist.
     * @param karte die Trumpfkarte
     */
    void notifyTrumpfSet(Karte karte) throws RemoteException;

    /**
     * Teilt den SpielRObservern mit, ein Teilnehmer hat das Spiel verlassen und es kann nicht weitergehen.
     * @param teilnehmer der gegangene Teilnehmer, um allen anderen mitzuteilen
     */
    void notifySpielGebrochen(Teilnehmer teilnehmer) throws RemoteException;

    /**
     * Liefert den aktuellen Spielstand des laufenden Spieles zurück
     * @return  Spielstand des aktuellen Spiels
     */
    Spielstand getSpielstand() throws RemoteException;

    /**
     * Gibt SpielerAnReihe zurück.
     * @return SpielerAnReihe
     */
    Teilnehmer getSpielerAnReihe() throws RemoteException;

    /**
     * Ruft countDown auf legeLatch, um Mitzuteilen, dass die Karte gelegt wurde.
     */
    void countDownLege() throws RemoteException;

    /**
     * Ruft countDown auf drueckeLatch, um Mitzuteilen, dass die Karte gedrueckt wurde.
     */
    void countDownDruecke() throws RemoteException;

    void ersetzeSpieler(String spielerName, Spiel spiel) throws RemoteException;
}
