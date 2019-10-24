/* Bereit für Abgabe */
package spiel;

import nutzer.Teilnehmer;

import java.io.Serializable;
import java.rmi.RemoteException;

/**
 * Der Spielstand eines Spieles.
 */
public interface Spielstand extends Serializable{

    /**
     * Gibt die Gewinnpunkte, des als Parameter uebergebenen Teilnehmers zurueck
     * @param teilnehmer    Teilnehmer, dessen Gewinnpunkte gewünscht sind
     * @return              Höhe der Gewinnpunkte
     */
    int getGewinnpunkte(Teilnehmer teilnehmer) throws RemoteException;

    /**
     * Gibt die Summe der Gewinnpunkte aller Teilnehmer aus der Spielpartie zurück.
     * @return  Summe der Gewinnpunkte aller Teilnehmer
     */
    int getAlleStichpunkte() throws RemoteException;

    /**
     * Gets DrueckPunkte.
     * @param teilnehmer dessen DrueckPunkte get werden muessen.
     * @return
     */
    int getDrueckStichpunkte(Teilnehmer teilnehmer) throws RemoteException;

    /**
     * Fuegt DrueckStichpunkte dem Teilnehmer für die Karte zu.
     * @param teilnehmer dem die DrueckStichpunkte gegeben werden.
     * @param karte für die die DrueckStichpunkte gegeben werden.
     */
    void zufuegeDrueckStichpunkte(Teilnehmer teilnehmer, Karte karte) throws RemoteException;

    /**
     * Gibt die Stichpunkte, des als Parameter uebergebenen Teilnehmers zurueck
     * @param teilnehmer    Teilnehmer, dessen Stichpunkte gewünscht sind
     * @return              Höhe der Stichpunkte
     */
    int getStichpunkte(Teilnehmer teilnehmer) throws RemoteException;

    /**
     * Setzt die Stichpunkte aller Teilnehmer des Spieles zurueck auf den Initialwert von 0
     */
    void resetSpielpartie() throws RemoteException;

    /**
     * Fuegt die angegebenen Punkte dem angegebenen Teilnehmer als Gewinnpunkte hinzu
     * @param teilnehmer    Teilnehmer, der die Gewinnpunkte bekommen soll
     * @param punkte        Anzahl der Gewinnpunkte
     */
    void zufuegeGewinnpunkte(Teilnehmer teilnehmer, int punkte) throws RemoteException;

    /**
     * Fügt die dem Wert der Karte entsprechenden Punkte dem angegebenen Teilnehmer als Stichpunkte hinzu
     * @param teilnehmer    der Teilnehmer, der die Gewinnpunkte bekommt
     * @param karte        Anzahl an Gewinnpunkte
     */
    void zufuegeStichpunkte(Teilnehmer teilnehmer, Karte karte) throws RemoteException;

    /**
     * Fügt dem Stichgewinner die zusätzliche 5 Stichpunkte.
     * @param stichgewinner der Teilnehmer, der den Stich gewonnen hat
     */
    void zufuegeStichpunkteLetzterStich(Teilnehmer stichgewinner) throws RemoteException;

    /**
     * Setzt die Trumpfkarte. Wird benötigt um die korrekten Punkte auf die Spieler aufzuteilen
     * @param karte             Karte, die den Trumpf definiert  Wenn die Registry nicht erreicht werden kann
     */
    void setTrumpf(Karte karte) throws RemoteException;

    /**
     * Gibt die Trumpf-Karte zurück.
     * @return Trumpf-Karte
     */
    Karte getTrumpf() throws RemoteException;

    void ersetzeTeilnehmer(Teilnehmer teilnehmer, Teilnehmer bot);

}
