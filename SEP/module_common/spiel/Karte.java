/* Bereit für Abgabe */
package spiel;

import java.io.Serializable;

/**
 * Stellt eine Karte dar.
 */
public interface Karte extends Serializable {

    /**
     * Gibt die Farbe der Karte zurück.
     * @return Farbe der Karte
     */
    Farbe getFarbe();

    /**
     * Gibt das Symbol der Karte zurück.
     * @return Symbol der Karte
     */
    Symbol getSymbol();
}
