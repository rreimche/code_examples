package spiel;
import nutzer.Bot;
import nutzer.Nutzer;
import nutzer.Spieler;
import nutzer.Teilnehmer;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;

/**
 * Implementationsklasse für Spiel.
 */
public class SpielImpl extends UnicastRemoteObject implements Spiel{
    private final String name;
    private final Spielpartie spielpartie;
    private List<SpielRObserver> spielRObservers = new ArrayList<>();
    private final Spielstand spielstand;
    private List<Teilnehmer> teilnehmer = new ArrayList<>();
    private List<Nutzer> nutzer = new ArrayList<>();
    private CountDownLatch legeLatch;
    private CountDownLatch drueckeLatch;

    public SpielImpl (List<Teilnehmer> teilnehmerList, List<Nutzer> nutzerList, String spielname) throws RemoteException{
        name = spielname;
        teilnehmer = new ArrayList<>(teilnehmerList);
        nutzer = new ArrayList<>(nutzerList);
        spielstand = new SpielstandImpl();
        for(Teilnehmer t : teilnehmerList){
            if(t instanceof Bot){
                Bot b = (Bot)t;
                b.setSpiel(this);
                b.setSpielstand(spielstand);
            }
        }
        spielpartie = new SpielpartieImpl(teilnehmerList, spielstand, this);
    }

    public SpielImpl (List<Teilnehmer> teilnehmerList, List<Nutzer> nutzerList, String spielname, Spielstand spielstand, Spielpartie spielpartie) throws RemoteException{
        this.name = spielname;
        this.spielstand = spielstand;
        this.spielpartie = spielpartie;
        this.teilnehmer = teilnehmerList;
        this.nutzer = nutzerList;
    }

    @Override
    public List<Teilnehmer> getTeilnehmerList() {
        return teilnehmer;
    }

    @Override
    public void spielStarten(List<SpielRObserver> spielRObserverList) {
        Spiel spiel = this;
        this.spielRObservers = spielRObserverList;
        for (SpielRObserver spielRObserver : spielRObserverList) {
            new Thread(new Runnable() {
                @Override
                public void run() {
                    try {
                        while (!obSpielgewinnerBestimmt()) {
                            spielRObserver.ping("nochda");
                            Thread.sleep(30000);
                        }
                    } catch (InterruptedException e) {
                        //nichts
                    } catch (RemoteException e) {
                        String tempName = null;
                        spielRObservers.remove(spielRObserver);
                        List<String> stringTeilnehmer = new ArrayList<>();
                        List<String> stringSpielRObserver = new ArrayList<>();
                        try {
                            for(SpielRObserver s : spielRObserverList){
                                stringSpielRObserver.add(s.getIdentifier());
                            }
                            for (Teilnehmer t : teilnehmer){
                                stringTeilnehmer.add(t.getName());
                            }
                            for (int i = 0; i < stringTeilnehmer.size(); i++){
                                if (!stringSpielRObserver.contains(stringTeilnehmer.get(i))){
                                    tempName = stringTeilnehmer.get(i);
                                    stringTeilnehmer.remove(i);
                                }
                            }
                            ersetzeSpieler(tempName, spiel);
                        }
                        catch (RemoteException e1) {
                            e1.printStackTrace();
                        }
                    }
                }
            }).start();
        }

        while(!obSpielgewinnerBestimmt() ) {
            spielpartie.spielpartieDurchfueren();
        }
        spielBeenden();
    }

    @Override
    public void notifyLosGehts(){
        drueckeLatch = new CountDownLatch(spielRObservers.size());
        try {
            for (SpielRObserver o : spielRObservers) {
                o.notifyLosGehts(this);
            }
        }
        catch (RemoteException e){
            e.printStackTrace();
        }
    }

    @Override
    public void notifyBereitzumDruecken() {
        /*CountDownLatch startLatch = new CountDownLatch(1);
        CountDownLatch readyLatch = new CountDownLatch(spielRObservers.size());

        for(SpielRObserver o : spielRObservers){
            new Thread(() -> {
                try{ startLatch.await(); } catch (InterruptedException e){ e.printStackTrace(); }

                try{ o.notifyBereitZumDruecken(this); } catch (RemoteException e){ e.printStackTrace(); }

                readyLatch.countDown();

            }).start();
        }

        startLatch.countDown();

        try{ readyLatch.await(); } catch (InterruptedException e){ e.printStackTrace(); }*/

        //RemoteCountDownLatch drueckeLatch = new RemoteCountDownLatch(3);
        drueckeLatch = new CountDownLatch(spielRObservers.size());
        try {
            for (SpielRObserver o : spielRObservers) {
                o.notifyBereitZumDruecken(this);
            }
            drueckeLatch.await();
            for (Teilnehmer t : teilnehmer) {
                if(t instanceof Bot){
                    Thread.sleep(500);
                    Bot b = (Bot)t;
                    b.drueckeKarte();
                }
            }
        }
        catch (RemoteException | InterruptedException e){ e.printStackTrace();}
    }


    @Override
    public void notifyShowKarteGedruecktAndere(String spielername, Karte karte) throws RemoteException {
        for(SpielRObserver spielRObserver : spielRObservers){
            spielRObserver.notifyShowKarteGedruecktAndere(spielername, karte);
        }
    }

    @Override
    public void notifyShowKarteGelegt(String spielername, Karte karte) throws RemoteException{
        for(SpielRObserver spielRObserver : spielRObservers){
            spielRObserver.notifyKarteGelegtAndere(spielername, karte);
        }
    }

    @Override
    public void notifyEndeSpielpartie(String teilnehmer0, int gewinnpunkteTeilnehmer0, String teilnehmer1, int gewinnpunkteTeilnehmer1, String teilnehmer2, int gewinnpunkteTeilnehmer2) throws RemoteException {
        CountDownLatch startLatch = new CountDownLatch(1);
        CountDownLatch readyLatch = new CountDownLatch(spielRObservers.size());

        for(SpielRObserver o : spielRObservers){
            new Thread(() -> {
                try{ startLatch.await(); } catch (InterruptedException e){ e.printStackTrace(); }

                try{ o.notifyEndeSpielpartie(this, teilnehmer0, gewinnpunkteTeilnehmer0, teilnehmer1, gewinnpunkteTeilnehmer1, teilnehmer2, gewinnpunkteTeilnehmer2);
                } catch (RemoteException e){ e.printStackTrace(); }

                readyLatch.countDown();

            }).start();
        }

        startLatch.countDown();

        try{ readyLatch.await(); } catch (InterruptedException e){ e.printStackTrace(); }

    }

    @Override
    public void notifyEndeStich(String teilnehmer,List<String> teilnehmerStichpunkteList) {
        CountDownLatch startLatch = new CountDownLatch(1);
        CountDownLatch readyLatch = new CountDownLatch(spielRObservers.size());

        for(SpielRObserver o : spielRObservers){
            new Thread(() -> {
                try{ startLatch.await(); } catch (InterruptedException e){ e.printStackTrace(); }

                try{ o.notifyEndeStich(this, teilnehmer, teilnehmerStichpunkteList);  } catch (RemoteException e){ e.printStackTrace(); }

                readyLatch.countDown();

            }).start();
        }

        startLatch.countDown();

        try{ readyLatch.await(); } catch (InterruptedException e){ e.printStackTrace(); }
    }

    @Override
    public void notifyKarteGelegt(String teilnehmer, Karte karte) {
        CountDownLatch startLatch = new CountDownLatch(1);
        CountDownLatch readyLatch = new CountDownLatch(spielRObservers.size());

        for(SpielRObserver o : spielRObservers){
            new Thread(() -> {
                try{ startLatch.await(); } catch (InterruptedException e){ e.printStackTrace(); }

                try{ o.notifyKarteGelegt(this, teilnehmer, karte);  } catch (RemoteException e){ e.printStackTrace(); }

                readyLatch.countDown();

            }).start();
        }

        startLatch.countDown();

        try{ readyLatch.await(); } catch (InterruptedException e){ e.printStackTrace(); }
    }

    @Override
    public void notifyKarteVerteilt(Karte karte, String name) {
        CountDownLatch startLatch = new CountDownLatch(1);
        CountDownLatch readyLatch = new CountDownLatch(spielRObservers.size());
        for(SpielRObserver o : this.spielRObservers){
            new Thread(() -> {
                try{ startLatch.await(); } catch (InterruptedException e){ e.printStackTrace(); }
                try{ o.notifyKarteVerteilt(this, karte, name);  } catch (RemoteException e){ e.printStackTrace(); }
                readyLatch.countDown();
            }).start();
        }

        startLatch.countDown();

        try{ readyLatch.await(); } catch (InterruptedException e){ e.printStackTrace(); }
    }

    @Override
    public void notifySpielBereit(boolean bereit) {
        for( SpielRObserver o : spielRObservers){
            try{
                o.notifySpielBereit(this, bereit);
            } catch (RemoteException e){
                e.printStackTrace();
            }
        }
    }

    @Override
    public void notifySpielerAmZug(Teilnehmer teilnehmer) {
        legeLatch = new CountDownLatch(1);
        try {
            for (SpielRObserver o : spielRObservers) {
                o.notifySpielerAmZug(this, teilnehmer.getName());
            }
        } catch (RemoteException e){ e.printStackTrace();}

        try{
            legeLatch.await();
        } catch (InterruptedException e){ e.printStackTrace(); }
    }

    @Override
    public void notifyTrumpfSet(Karte karte) {
        CountDownLatch startLatch = new CountDownLatch(1);
        CountDownLatch readyLatch = new CountDownLatch(spielRObservers.size());

        for(SpielRObserver o : spielRObservers){
            new Thread(() -> {
                try{ startLatch.await(); } catch (InterruptedException e){ e.printStackTrace(); }

                try{ o.notifyTrumpfSet(this, karte);  } catch (RemoteException e){ e.printStackTrace(); }

                readyLatch.countDown();

            }).start();
        }

        startLatch.countDown();

        try{ readyLatch.await(); } catch (InterruptedException e){ e.printStackTrace(); }
    }

    /*@Override
    public void notifySpielBeendet(Spielstand spielstand) {
        for( SpielRObserver o : spielRObservers){
            try{
                o.notifySpielBeendet(spielstand);
            } catch (RemoteException e){
                e.printStackTrace();
            }
        }
    }*/

    @Override
    public void notifySpielGebrochen(Teilnehmer teilnehmer) {
        CountDownLatch startLatch = new CountDownLatch(1);
        CountDownLatch readyLatch = new CountDownLatch(spielRObservers.size());

        for(SpielRObserver o : spielRObservers){
            new Thread(() -> {
                try{ startLatch.await(); } catch (InterruptedException e){ e.printStackTrace(); }

                try{ o.notifySpielGebrochen(teilnehmer);  } catch (RemoteException e){ e.printStackTrace(); }

                readyLatch.countDown();

            }).start();
        }

        startLatch.countDown();

        try{ readyLatch.await(); } catch (InterruptedException e){ e.printStackTrace(); }
    }


    @Override
    public void addObserver(SpielRObserver spielRObserver) {
        spielRObservers.add(spielRObserver);
    }

    @Override
    public void removeObserver(SpielRObserver spielRObserver) throws RemoteException {
        spielRObservers.remove(spielRObserver);
    }

    @Override
    public void removeObserver(String name) throws RemoteException{
        for(int i = 0; i< spielRObservers.size(); i++) {
                if (spielRObservers.get(i).getIdentifier().equals(name)) {
                    spielRObservers.remove(i);
                }
        }
            //spielRObservers.remove(spielRObserver);
    }

    @Override
    public Spielstand getSpielstand() {
        return spielstand;
    }

    @Override
    public Teilnehmer getSpielerAnReihe() throws RemoteException {
        return spielpartie.getSpielerAnReihe();
    }

    @Override
    public void countDownLege() throws RemoteException {
        legeLatch.countDown();
    }

    @Override
    public void countDownDruecke() throws RemoteException {
        drueckeLatch.countDown();
    }

    public void ersetzeSpieler(String spielerName, Spiel spiel) throws RemoteException {
        new Thread(new Runnable() {
            @Override
            public void run() {
                try {
                    Teilnehmer teilnehmerErsetzen = null;
                    for (Teilnehmer teilnehmer : getTeilnehmerList()){
                        if(teilnehmer.getName().equals(spielerName)){
                            teilnehmerErsetzen = teilnehmer;
                        }
                    }
                    for (SpielRObserver spielRObserver : spielRObservers) {
                        spielRObserver.notifySpielGebrochen(teilnehmerErsetzen);
                    }
                    if (botDrin() == 0) {     //Kein Bot in Spiel
                        int index = 0;
                        for(int i = 0; i < teilnehmer.size(); i++){
                            if (teilnehmer.get(i).getName().equals(spielerName)){
                                index = i;
                                teilnehmer.remove(i);
                                break;
                            }
                        }
                        Bot bot = new Bot("Bot1");
                        bot.setSpiel(spiel);
                        spielstand.ersetzeTeilnehmer(teilnehmerErsetzen, bot);
                        bot.setSpielstand(spielstand);
                        bot.setStichstapel(teilnehmerErsetzen.getStichstapel());
                        bot.setBlatt(teilnehmerErsetzen.getBlatt());
                        bot.setTrumpf(spielpartie.getTrumpf());
                        teilnehmer.add(index, bot);
                        spielpartie.aktualisiereSpielerListe(spielerName, bot);
                        for (SpielRObserver spielRObserver : spielRObservers) {
                            spielRObserver.notifyChangeLabel(teilnehmerErsetzen.getName(), "Bot1");
                        }
                        if (bot.getBlattList().size() == 12){
                            bot.drueckeKarte();
                        }
                        if(spielpartie.getSpielerAnReihe().getName().equals("Bot2")){
                            bot.karteAusspielen();
                        }
                    }
                    if (botDrin() == 1) {
                        int index = 0;
                        for(int i = 0; i < teilnehmer.size(); i++){
                            if (teilnehmer.get(i).getName().equals(spielerName)){
                                index = i;
                                teilnehmer.remove(i);
                                break;
                            }
                        }
                        Bot bot = new Bot("Bot2");
                        bot.setSpiel(spiel);
                        spielstand.ersetzeTeilnehmer(teilnehmerErsetzen, bot);
                        bot.setSpielstand(spielstand);
                        bot.setStichstapel(teilnehmerErsetzen.getStichstapel());
                        bot.setBlatt(teilnehmerErsetzen.getBlatt());
                        bot.setTrumpf(spielpartie.getTrumpf());
                        teilnehmer.add(index, bot);
                        spielpartie.aktualisiereSpielerListe(spielerName, bot);
                        for (SpielRObserver spielRObserver : spielRObservers) {
                            spielRObserver.notifyChangeLabel(teilnehmerErsetzen.getName(), "Bot2");
                        }
                        if (bot.getBlattList().size() == 12){
                            //bot.drueckeKarte();
                            countDownDruecke();
                        }
                        if(spielpartie.getSpielerAnReihe().getName().equals(teilnehmerErsetzen.getName())){
                            //bot.karteAusspielen();
                            countDownLege();
                        }

                    } else {
                        //Spiel beenden
                    }
                }catch (RemoteException e){
                    e.printStackTrace();
                }
            }
        }).start();
    }

    private int botDrin(){
        int i = 0;
        for (Teilnehmer t : getTeilnehmerList()){
            if (t instanceof Bot){
                i++;
            }
        }
        return i;
    }

    private void spielBeenden(){
        for(Teilnehmer t : teilnehmer){
            try {
                if (t instanceof Spieler){
                    Nutzer n = ((Spieler)t).getNutzer();
                    if ( 7 == spielstand.getGewinnpunkte(t) ) { //if it is Spielgewinner
                        n.aktualisiereStatistik(true);

                    } else {
                        n.aktualisiereStatistik(false);
                    }
                }
            } catch ( RemoteException e) { e.printStackTrace(); }
        }

        for( SpielRObserver o : spielRObservers){
            try{
                Spielstand spielstand = getSpielstand();
                o.notifySpielBeendet(getTeilnehmerList().get(0).getName(), spielstand.getGewinnpunkte(getTeilnehmerList().get(0)), getTeilnehmerList().get(1).getName(), spielstand.getGewinnpunkte(getTeilnehmerList().get(1)), getTeilnehmerList().get(2).getName(), spielstand.getGewinnpunkte(getTeilnehmerList().get(2)));
            } catch (RemoteException e){
                e.printStackTrace();
            }
        }
    }

    private boolean obSpielgewinnerBestimmt(){
        for( Teilnehmer t : teilnehmer){
            try {
                if( 7 ==  spielstand.getGewinnpunkte(t) ) return true;
            } catch (RemoteException e) {
                e.printStackTrace();
            }
        }

        return false;
    }
}