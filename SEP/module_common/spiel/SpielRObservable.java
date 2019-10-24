/* Bereit für Abgabe */
package spiel;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * Observable, um mit SpielRObservern zusammen zu arbeiten.
 */
public interface SpielRObservable extends Remote {
    /**
     * Fügt einen SpielRObserver hinzu, der bei Änderungen benachrichtigt werden soll
     * @param spielRObserver    Der hinzuzufuegende SpielRObserver
     */
    void addObserver(SpielRObserver spielRObserver) throws RemoteException;

    void removeObserver(SpielRObserver spielRObserver) throws RemoteException;
    /**
     * Entfernt einen SpielRObserver, sodass dieser nicht mehr bei Aenderungen durch das Observable benachrichtigt wird
     //* @param name    Der zu entfernende SpielRObserver
     */
    void removeObserver(String name) throws RemoteException;
}
