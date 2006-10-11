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
package alice.tuprolog.lib;
import alice.tuprolog.*;
import alice.tuprolog.Number;

/**
 * Library for managing DCGs.
 * 
 * Library/Theory dependency: BasicLibrary
 *
 *
 *
 */
public class DCGLibrary extends Library {

    public DCGLibrary(){
    }

    public String getTheory(){
        return
            //
            // operators defined by the BasicLibrary theory
            //
            ":- op( 1200, xfx,  '-->'). \n"+
            "dcg_nonterminal(X) :- list(X),!,fail.  \n"+
            "dcg_nonterminal(_).                    \n"+
            "dcg_terminals(Xs) :- list(Xs).         \n"+
            "phrase(Category,String,Left) :- dcg_parse(Category,String\\Left).      \n"+
            "phrase(Category,[H|T]) :- dcg_parse(Category,[H|T]\\[]).      \n"+
            "dcg_parse(A,Tokens) :- dcg_nonterminal(A), (A --> B), dcg_parse(B,Tokens).            \n"+
            "dcg_parse((A,B),(Tokens \\ Xs)) :- dcg_parse(A,(Tokens \\ Tokens1)), dcg_parse(B,(Tokens1 \\ Xs)).    \n"+
            "dcg_parse(A,Tokens) :- dcg_terminals(A), dcg_connect(A,Tokens).    \n"+
            "dcg_parse({A},(Xs \\ Xs)) :- A. \n"+
            "dcg_connect([],(Xs \\ Xs)). \n"+
            "dcg_connect([W|Ws],([W|Xs] \\ Ys)) :- dcg_connect(Ws,(Xs \\ Ys)). \n";
    }
}