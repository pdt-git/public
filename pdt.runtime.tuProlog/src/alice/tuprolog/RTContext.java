/*
 * tuProlog - Copyright (C) 2001-2002  aliCE team at deis.unibo.it
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package alice.tuprolog;
import java.io.Serializable;

/**
 * This class defines the runtime execution context
 * of a prolog engine during a demonstration.
 * <p>
 * it keeps track of information necessary
 * to do a correct renaming of variables (time)
 * and a correct unification/deunification of
 * terms (mark)
 * <p>
 * it manages information about current
 * demonstrating goal, current choice explored,
 * open choices
 *
 *
 *
 */
class RTContext implements Serializable {
    /**
     * dbase before goal evaluation - it's a list of assert and retract ops
     * done before
     */
    alice.util.LinkedList     sDBTS;

    /** starting goal */
    public Term      startGoal;
    /** current goal */
    public GoalInfo currentGoal;
    /** starting level, time and mark */
    public int         startLevel,startTime,startMark;
    // current time and mark index
    public int time,mark;
    /** evaluation state: false -> not demonstrable goal */
    public boolean     evalState;
    /** clause compatible with goal list */
    public alice.util.LinkedList  compatibleGoals;
    public int nCompatibleGoals;
    /** goals to execute list */
    public alice.util.LinkedList  goalsToEval;
    public int nGoalsToEval;
    /** terms executed list */
    public alice.util.LinkedList  termsExecuted;
    /** number of choices explored */
    public int nTermsExecuted;
    /** number of choices not yet explored */
    public int nOpenTermsExecuted;

    private static int nRenamedVarTime = 1000000;
    
    public RTContext(int startRenamedVarTime){
    		nRenamedVarTime = startRenamedVarTime;
    }

    public RTContext(){
    }
    
    /** saves current goal to goals to be executed list */
    public void saveFGoal() {
        goalsToEval = new alice.util.LinkedList(currentGoal,goalsToEval);
        nGoalsToEval++;
    }

    /** fetches next goal to be executed */
    public void loadFGoal() {
        currentGoal = (GoalInfo)goalsToEval.head;
        goalsToEval = goalsToEval.tail;
        nGoalsToEval--;
        currentGoal.time = time;
        currentGoal.mark = mark;
    }

    /** saves current goal to executed goals list */
    public void saveTGoal() {
        termsExecuted = new alice.util.LinkedList(currentGoal,termsExecuted);
        nTermsExecuted++;
        if(currentGoal.context != null){
            nOpenTermsExecuted++;
        }
    }

    /** fetches next goal from executed goal list (backtrack phase) */
    public void loadTGoal() {
        currentGoal = (GoalInfo)termsExecuted.head;
        termsExecuted = termsExecuted.tail;
        nTermsExecuted--;
        time = currentGoal.time;
        mark = currentGoal.mark;
        if(currentGoal.context != null){
            nOpenTermsExecuted--;
        }
    }

    /** sets current goal */
    public void setGoal(Term g) {
        currentGoal = new GoalInfo(g);
    }

    /**
     * cuts current SLD tree
     * <p>
     * cuts every open alternative of subgoals,
     * empty the open alternatives list
     */
    public void cut() {
        alice.util.LinkedList l = termsExecuted;
        while(!l.isEmptyList()) {
            ((GoalInfo)l.head).context = null;
            l = l.tail;
        }
        compatibleGoals = new alice.util.LinkedList();
        nCompatibleGoals = 0;
        nOpenTermsExecuted = 0;
    }

    /** loads the time and mark index of current goal */
    public void loadStatus() {
        time = currentGoal.context.time;
        mark = currentGoal.context.mark;
    }

    /**
     * gets the first open alternatives if exist and try to unify with current goal
     */
    public boolean loadChoice() {
        if(nCompatibleGoals == 0){
            return(false);
        }
        ClauseInfo d = (ClauseInfo)compatibleGoals.head;
        //System.out.println("LOADING CHOICE "+d.clause);
        compatibleGoals = compatibleGoals.tail;
        nCompatibleGoals--;
        startGoal.unify(d.clause.getArg(0),startMark);
        currentGoal  = new GoalInfo(d.clause.getArg(1));
        goalsToEval = new alice.util.LinkedList();
        nGoalsToEval = 0;
        // current Time is next time avaiable after clause's one
        time  = d.timestamp;
        
        //System.out.println("LOAD CHOICE timestamp "+time);
        
        //System.out.println("LOADING CHOICE "+startGoal+" time "+time);
        mark  = startMark + 1;
        // loading current goal in goals to be executed list
        saveFGoal();
        return(true);
    }

    /**
     * returns true if at least other solution path exists
     */
    public boolean existChoice() {
        return(nCompatibleGoals + nOpenTermsExecuted != 0);
    }

    /**
     * gets a copy of a term renaming the vars
     * starting from current time index
     * <p>
     * VERY IMPORTANT:  in order to get a correct
     * renaming process, time count is updated reflecting
     * eventually new variables encountered in the term
     * to be renamed
     */
    public Term getRenamedTermCopy(Term t) {
        alice.util.LinkedList l = new alice.util.LinkedList();
        t = t.copy(l);
        //time = Var.rename(l,time+1,true);
        nRenamedVarTime = Var.rename(l,nRenamedVarTime+1,true);
        return(t);
    }

    public String toString(){
        String st =  "[ Runtime context -- "+
                     "starting goal: "+startGoal+
                     " -- current goal: "+currentGoal+
                     " -- start level, time, mark: "+startLevel+" "+startTime+" "+startMark+
                     " -- current time, mark: "+time+" "+mark+
                     " -- eval state: "+evalState+
                     " -- number of terms executed: "+nTermsExecuted+
                     " -- number of open choices: "+nOpenTermsExecuted+
                     " -- compatible goal list: {";
        alice.util.LinkedList list = compatibleGoals;
        while (!list.isEmptyList()){
            st+=list.getHead().toString()+", ";
            list = list.getTail();
        }

        st+="} -- goals to evaluate list: {";
        list = goalsToEval;
        while (!list.isEmptyList()){
            st+=list.getHead().toString()+", ";
            list = list.getTail();
        }

        st+="} -- terms executed list: {";
        list = termsExecuted;
        while (!list.isEmptyList()){
            st+=list.getHead().toString()+", ";
            list = list.getTail();
        }
        st+="} ]";
        return st;
    }

}