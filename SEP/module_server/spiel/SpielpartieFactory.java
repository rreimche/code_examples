package spiel;

import nutzer.Teilnehmer;

import java.util.List;

/**
 * Factory-Class für Spielpartien.
 */
public interface SpielpartieFactory {

    /**
     * Erstellt eine Spielpartie mit gegebenen Parametern.
     * @param teilnehmerList eine List von Teilnehmern
     * @param spielstand     ein Spielstand
     * @param spiel          ein Spiel
     * @return               die erstellte Spielpartie
     */
    static Spielpartie erstelleSpielpartie(List<Teilnehmer> teilnehmerList, Spielstand spielstand, Spiel spiel) {

        return new SpielpartieImpl(teilnehmerList, spielstand, spiel);
    }
}
