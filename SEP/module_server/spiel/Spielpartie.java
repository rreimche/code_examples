package spiel;

import nutzer.Teilnehmer;

import java.rmi.RemoteException;
import java.util.List;

/**
 * Eine Partie eines Spieles. Dient der Verwaltung einer/mehrerer Spielpartien.
 */
public interface Spielpartie{

    /**
     * Gibt die aktuelle Trumpfkarte der Spielpartie zurück.
     * @return  Karte, die die Trumpfkarte bestimmt
     */
    Karte getTrumpf();

    /**
     * Verwaltet und initiiert das Durchführen einer Spielpartie.
     */
    void spielpartieDurchfueren();

    /**
     * Gibt SpielerAnReihe zurück.
     * @return SpielerAnReihe
     */
    Teilnehmer getSpielerAnReihe();

    /**
     * Aktualisiert die Spielerliste der Spielpartie, für den Fall, wenn ein Spieler das laufende Spiel verlassen hat
     * @param zuersetzenderTeilnehmer       Teilnehmer, der ersetzt werden soll
     * @param t2                            Teilnehmer, der den Teilnehmer erstezt, der das Spiel verlassen hat
     * @throws RemoteException              Wenn die Registry nicht erreicht werden kann
     */
    void aktualisiereSpielerListe(String zuersetzenderTeilnehmer, Teilnehmer t2) throws RemoteException;
}
