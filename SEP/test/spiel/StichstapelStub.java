package spiel;

import exceptions.KarteUnlegbarException;
import exceptions.SpielerNichtAmZugException;
import javafx.util.Pair;
import nutzer.Teilnehmer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Created by rreimche on 17.06.15.
 */
public class StichstapelStub implements Stichstapel {
    private boolean abgeschlossen, abgeschlossen_letzte, reset, trumpfset;
    private Teilnehmer stichgewinner;
    private List<Pair<Teilnehmer, Karte>> stichstapel = new ArrayList<>();

    public StichstapelStub(){}

    public StichstapelStub(Teilnehmer s){
        stichgewinner = s;
    }

    @Override
    public Teilnehmer abschliesseStich() {
        abgeschlossen = true;
        return stichgewinner;
    }

    @Override
    public Teilnehmer abschliesseLetztenStich() {
        abgeschlossen_letzte = true;
        return stichgewinner;
    }

    @Override
    public void aendereSpielstand(Teilnehmer t, Karte k) {

    }

    @Override
    public void drueckeKarte(Teilnehmer teilnehmer, Karte karte) {

    }

    @Override
    public Karte getTrumpf() {
        return null;
    }

    @Override
    public void legeKarte(Karte karte, Teilnehmer teilnehmer, Teilnehmer spielerAmReihe) throws KarteUnlegbarException, SpielerNichtAmZugException {
        stichstapel.add(new Pair<>(teilnehmer, karte));
    }

    @Override
    public void resetStichstapel() {
        reset = true;
    }

    @Override
    public void setTrumpf(Karte k) {
        trumpfset = true;
    }

    @Override
    public List<Karte> getGelegteKarten() {
        List<Karte> result = new ArrayList<>();
        for(Pair<Teilnehmer,Karte> k : stichstapel){
            result.add(k.getValue());
        }

        return result;
    }

    @Override
    public List<Karte> getLegbareKarten(List<Karte> karteList) {
        return karteList;
    }

    @Override
    public boolean obKarteLegbar(Karte karte, List<Karte> karteList) {
        return false;
    }

    public boolean obAbgeschlossen(){
        return abgeschlossen;
    }

    public boolean obReset(){
        return reset;
    }

    public boolean obTrumpfSet(){
        return trumpfset;
    }
}
