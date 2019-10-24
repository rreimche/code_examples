/* Bereit für Abgabe */
package spiel;

import nutzer.Teilnehmer;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.List;

/**
 * Observer für SpielRObservable.
 */
public interface SpielRObserver extends Remote {
    /**
     * Teilt dem Client mit, dass alle Teilnehmer jetzt eine Karte drücken dürfen.
     * @param spielRObservable      SpielROvservable-Objekt, um mitzuteilen, von wem man benachrichtigt wurde
     */
    void notifyBereitZumDruecken(SpielRObservable spielRObservable) throws RemoteException;

    /**
     * Teilt dem Client mit, dass das Spiel los gegangen ist.
     * @param spielRObservable
     */
    void notifyLosGehts(SpielRObservable spielRObservable) throws RemoteException;

    /**
     * Benachrichtigt die anderen Spieler/Observer, dass ein Mitspieler eine Karte gedrückt hat
     * @param spielername       Name des Spielers, der eine Karte gedrückt hat
     * @param karte             Karte, die den entsprechende Spieler gedrückt hat
     */
    void notifyShowKarteGedruecktAndere(String spielername, Karte karte) throws RemoteException;

    /**
     * Teilt dem Client mit, dass das Spiel angefangen wurde.
     * @param spielRObseravble SpielROvservable-Objekt, um mitzuteilen, von wem man benachrichtigt wurde
     * @param teilnehmerList List von Teilnehmer, um Client zu übergeben
     */
    void notifySpielAngefangen(SpielRObservable spielRObseravble, List<Teilnehmer> teilnehmerList) throws RemoteException;

    /**
     * Benachrichtigt die anderen Clienten darüber, welcher Spieler welche Karte gedrückt hat
     * @param spielername       Name des Spielers, der die Karte gedrückt hat
     * @param karte             Karte, die der entsprechende Spieler gelegt hat
     */
    void notifyKarteGelegtAndere(String spielername, Karte karte) throws RemoteException;

    /**
     * Teilt den Clienten bzw. den Observern mit, dass die aktuelle Spielpartie zu Ende ist.
     * @param spielRObservable              Observable-Objekt, das uebergeben wird, um zu wissen, von wem man benachrichtigt wurde
     * @param teilnehmer0                   Erster Teilnehmer
     * @param gewinnpunkteTeilnehmer0       Gewinnpunkte des ersten Teilnehmers
     * @param teilnehmer1                   Zweiter Teilnehmer
     * @param gewinnpunkteTeilnehmer1       Gewinnpunkte des zweiten Teilnehmers
     * @param teilnehmer2                   Dritter Teilnehmer
     * @param gewinnpunkteTeilnehmer2       Gewinnpunkte des dritten Teilnehmers
    Wenn die Registry nicht kontaktiert werden konnte
     */
    void notifyEndeSpielpartie(SpielRObservable spielRObservable, String teilnehmer0, int gewinnpunkteTeilnehmer0, String teilnehmer1, int gewinnpunkteTeilnehmer1, String teilnehmer2, int gewinnpunkteTeilnehmer2) throws RemoteException;

    /**
     * Teilt dem Client mit, dass der aktuelle Stich zu Ende ist.
     * @param spielRObservable  Observable-Objekt, das uebergeben wird, um zu wissen, von wem man benachrichtigt wurde
     * @param teilnehmer        Teilnehmer,
     */
    void notifyEndeStich(SpielRObservable spielRObservable, String teilnehmer,List<String> teilnehmerStichpunkteList) throws RemoteException;

    /**
     * Teilt dem Client mit, dass ein Karte gelegt wurde.
     * @param spielRObservable  Observable-Objekt, das uebergeben wird, um zu wissen, von wem man benachrichtigt wurde
     * @param teilnehmer        Teilnehmer, der die Karte gelegt hat
     * @param karte             Die entsprechend gelegte Karte
     */
    void notifyKarteGelegt(SpielRObservable spielRObservable, String teilnehmer, Karte karte) throws RemoteException;

    /**
     * Teilt dem Client mit, welche Karte der Spieler bekommen hat
     * @param spielRObservable  Observable-Objekt, das uebergeben wird, um zu wissen, von wem man benachrichtigt wurde
     * @param karte             Karte, die der Spieler im Zuge des Kartenverteilens bekommt
     * @param name              Name des Spielers
     */
    void notifyKarteVerteilt(SpielRObservable spielRObservable, Karte karte, String name) throws RemoteException;

    /**
     * Teilt dem Clienten des Anfuehrers mit, dass alle anderen Spieler bereit sind
     * @param spielRObservable  Observable-Objekt, das uebergeben wird, um zu wissen, von wem man benachrichtigt wurde
     * @param bereit            Wert, der angibt, ob alle anderen Spieler bereit sind, sodass das Spiel dann gestartet
     *                          werden kann
     */
    void notifySpielBereit(SpielRObservable spielRObservable, boolean bereit) throws RemoteException;

    /**
     * Teilt dem Client mit, das der als Parameter uebergebene Teilnehmer nun am Zug ist
     * @param spielRObservable  Observable-Objekt, das uebergeben wird, um zu wissen, von wem man benachrichtigt wurde
     * @param teilnehmer        Name des Teilnehmers, der am Zug ist
     */
    void notifySpielerAmZug(SpielRObservable spielRObservable, String teilnehmer) throws RemoteException;

    /**
     * Pingt.
     * @param nachricht Eine Nachricht.
     * @throws RemoteException
     */
    void ping(String nachricht) throws RemoteException;

    /**
     * Teilt dem Client mit, welche Karte jetzt als Trumpf bestimmt ist.
     * @param spielRObservable SpielROvservable, keine Ahnung wofür ;)
     * @param karte dei Trumpfkarte
     */
    void notifyTrumpfSet(SpielRObservable spielRObservable, Karte karte) throws RemoteException;


    /**
     * Teilt dem Client mit, dass das Spiel beendet ist.
     * @param teilnehmer1   Wie gemeint.
     * @param punkteTeilnehmer1 Wie gemeint.
     * @param teilnehmer2   Wie gemeint.
     * @param punkteTeilnehmer2 Wie gemeint.
     * @param teilnehmer3   Wie gemeint.
     * @param punkteTeilnehmer3 Wie gemeint.
     */
    void notifySpielBeendet(String teilnehmer1, int punkteTeilnehmer1, String teilnehmer2, int punkteTeilnehmer2, String teilnehmer3, int punkteTeilnehmer3) throws RemoteException;

    /**
     * Teilt dem Client mit, ein Teilnehmer habe das Spiel verlassen und es könne nicht weitergehen.
     * @param teilnehmer der gegangene Teilnehmer, um allen anderen mitzuteilen
     */
    void notifySpielGebrochen(Teilnehmer teilnehmer) throws RemoteException;

    /**
     * Ändert Label
     * @param alterName alter Name
     * @param neuerName neuer Name
     */
    void notifyChangeLabel(String alterName, String neuerName) throws RemoteException;

    String getIdentifier() throws RemoteException;
}
