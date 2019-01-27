package java8news;

import java.util.List;

/**
 * Created by ldcsb on 01/30/2017.
 */
public class Album {

    private String mainMusician;
    private List<String> tracks;

    public void setMainMusician(String mainMusician) {
        this.mainMusician = mainMusician;
    }

    public String getMainMusician() {
        return mainMusician;
    }

    public void setTracks(List<String> tracks) {
        this.tracks = tracks;
    }

    public List<String> getTracks() {
        return tracks;
    }

    public double getTracksSize(){
        return tracks.size();
    }
}
