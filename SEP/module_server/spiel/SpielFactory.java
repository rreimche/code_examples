package spiel;

import nutzer.Nutzer;
import nutzer.Teilnehmer;

import java.rmi.RemoteException;
import java.util.List;

/**
 * Factory-Class für Spiel.
 */
public interface SpielFactory {

    /**
     * Erstellt ein spiel mit gegebenen Ahängigkeiten.
     * @param teilnehmerList    Ein Liste von Teilnehmern, die das Spiel spielen werden
     * @param nutzerList        Eine Liste von Nutzern, die das Spiel spielen werden
     * @param spielstand        Aktueller Spielstand für das Spiel
     * @param spielpartie       Aktuelle Spielpartie des Spiels
     * @param name              Name des Spieles
     * @return das erzeugte spiel mit den gegebenen Abhängigkeiten
     */
    static Spiel erstelleSpiel(List<Teilnehmer> teilnehmerList, List<Nutzer> nutzerList, List<SpielRObserver> observer,
                                      Spielstand spielstand, Spielpartie spielpartie, String name) {
        Spiel spiel = null;
        try {
            spiel = new SpielImpl(teilnehmerList, nutzerList, name, spielstand, spielpartie);
        } catch (RemoteException e) {
            e.printStackTrace();
        }
        for( SpielRObserver o : observer){
            try{
                assert spiel != null;
                spiel.addObserver(o);
            } catch ( RemoteException e){
                e.printStackTrace();
            }
        }

        return spiel;
    }
}
