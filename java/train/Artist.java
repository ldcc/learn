package com.ldcstorm.training;

import java.util.List;

/**
 * Created by ldcsb on 01/30/2017.
 */
public class Artist {

    private List<String> members;

    public void setMembers(List<String> members) {
        this.members = members;
    }

    public List<String> getMembers() {
        return members;
    }

    public double getMemberCount() {
        return members.size();
    }

    public void printMembers() {
        System.out.println(members);
    }

    public boolean isSolo() {
        return getMembers().size() == 1;
    }
}
