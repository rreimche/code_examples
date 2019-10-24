package spiel;

import exceptions.KarteUnlegbarException;
import exceptions.SpielerNichtAmZugException;
import exceptions.StichstapelUnerwartetLeerException;
import nutzer.Teilnehmer;

import java.io.Serializable;
import java.util.List;

/**
 * Stapel, wohin Teilnehmer Karten legen.
 */
public interface Stichstapel{
    /**
     * Schließt den aktuellen Stich ab. Es wird der Stichgewinner bestimmt; anschließend werden ihm die entsprechenden
     * Stichpunkte zugewiesen.
     * @return  Teilnehmer, der den Stich gewonnen hat
     * @throws StichstapelUnerwartetLeerException   wenn Stichstapel leer ist.
     */
    Teilnehmer abschliesseStich() throws StichstapelUnerwartetLeerException;

    /**
     * Schließt den aktuellen Stich ab. Es wird der Stichgewinner bestimmt; anschließend werden ihm die entsprechenden
     * Stichpunkte zugewiesen. Als
     * @return  Teilnehmer, der den Stich gewonnen hat
     * @throws StichstapelUnerwartetLeerException wenn Stichstapel leer ist.
     */
    Teilnehmer abschliesseLetztenStich() throws StichstapelUnerwartetLeerException;

    /**
     * Aendert den aktuellen Spielstand. Es werden dem angegebenen Teilnehmer die entsprechenden Stichpunkte,
     * die sich aus dem Wert der Karte ergeben, zugewiesen.
     * Die Anzahl der Stichpunkte hängt von der übergegebenen Karte ab.
     * @param teilnehmer    Teilnehmer, dem die Stichpunkte zugewiesen werden
     * @param karte         Karte, für die die Stichpunkte zugewiesen werden
     */
    void aendereSpielstand(Teilnehmer teilnehmer, Karte karte);

    /**
     * Drücke die Karte.
     * @param teilnehmer    Teilnehmer, der die Karte gedrückt hat
     * @param karte         Karte, die gedrückt werden soll
     */
    void drueckeKarte(Teilnehmer teilnehmer, Karte karte);

    /**
     * Gibt die Trumfkarte der aktuell laufenden Spielpartie zurück.
     * @return  Karte, die aktuell den Trumpf darstellt
     */
    Karte getTrumpf();

    /**
     * Legt die übergebene Karte in den Stichsstapel.
     * @param karte                         Die Karte, die gelegt werden soll
     * @param teilnehmer                    Der Teilnehmer, der die Karte legt
     * @param spielerAnReihe                Spieler, der aktuell an der Reihe ist
     * @throws KarteUnlegbarException       Wenn die Karte, die gelegt werden soll, nicht gelegt werden darf
     * @throws SpielerNichtAmZugException   Wenn der Spieler, der die Karte gelegt hat, gar nicht am Zug ist
     */
    void legeKarte(Karte karte, Teilnehmer teilnehmer, Teilnehmer spielerAnReihe) throws KarteUnlegbarException, SpielerNichtAmZugException;

    /**
     * Löscht alle im Stichstapel liegenden Karten und setzt den Trumpf zurück
     */
    void resetStichstapel();

    /**
     * Legt eine Karte als Trumpfkarte fest
     * @param karte     Karte, die die Trumpfkarte wird
     */
    void setTrumpf(Karte karte);

    /**
     * Gibt die Liste von schon gelegten Karten zurück. Die am letzten gelegte Karte hat den höchsten Index, die am
     * ersten, die niedrigste (0).
     * @return  Liste von schon gelegten Karten
     */
    List<Karte> getGelegteKarten();

    /**
     * Gibt alle spielbaren Karten aus karteList zurueck
     * @param karteList     Liste aller Karten, aus denen die legbaren Karten herausgefunden weden
     * @return Liste von legbaren Karten. Wird vom ursprünglich übergebenen Liste abgeleitet.
     */
    List<Karte> getLegbareKarten(List<Karte> karteList);

    /**
     * Gibt true zurueck, falls karte legbar ist, sonst false
     * @param karte         Karte, die überprüft werden soll, ob sie legbar ist
     * @param karteList     Stichstapel, der benutzt wird um zu überprüfen, ob die angegebene Karte legbar ist
     * @return true, wenn Karte legbar ist, sonst false.
     */
    boolean obKarteLegbar(Karte karte, List<Karte> karteList);
}
