/*Fertig für Abgabe*/
package spiel;

import exceptions.KarteNichtVorhandenException;
import exceptions.KarteSchonVorhandenException;

import java.io.Serializable;
import java.util.ArrayList;

/**
 * Stellt ein Blatt mit Karte, die ein Teilnehmer hat.
 */
public interface Blatt extends Serializable{
    /**
     * Liefert ein sortiertes Set an Karten zurueck
     * @return  ein SortedSet mit allen Karten, die im Blatt sind
     */
    ArrayList<Karte> getBlatt();

    /**
     * Entfernt die, als Parameter angegebene Karte aus dem Blatt.
     * @param karte     Karte, die aus dem Blatt entfernt werden soll
     */
    void karteEntfernen(Karte karte) throws KarteNichtVorhandenException;

    /**
     * Fuegt die als Parameter angegebene Karte dem Blatt hinzu
     * @param karte     Karte, die dem Blatt hinzugefügt werden soll
     */
    void karteZufuegen(Karte karte) throws KarteSchonVorhandenException;

    /**
     * Sortiert alle im Blatt vorhandenen Karten farblich absteigend.
     */
    void sortiereBlattFarblichAbsteig();

    /**
     * Sortiert alle im Blatt vorhandenen Karten farblich aufsteigend.
     */
    void sortiereBlattFarblichAufsteig();
}
