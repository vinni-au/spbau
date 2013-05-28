package ru.spbau.storozhev.drunksim.objects;

import java.util.Random;

import ru.spbau.storozhev.drunksim.ai.PathFinding;
import ru.spbau.storozhev.drunksim.core.Cell;
import ru.spbau.storozhev.drunksim.stepdecisions.AbstractStepDecision;
import ru.spbau.storozhev.drunksim.stepdecisions.DrinkerDropsBottleStepDecision;
import ru.spbau.storozhev.drunksim.stepdecisions.DrinkerLaysStepDecision;
import ru.spbau.storozhev.drunksim.stepdecisions.DrinkerSleepStepDecision;
import ru.spbau.storozhev.drunksim.stepdecisions.MoveStepDecision;

public class DrinkerCellObject extends AbstractCellObject {
	public enum DrinkerState {
		Walking,
		Sleeping,
		Laying
	}
	
	public DrinkerCellObject(Cell c) {
		super(c);
	}

	@Override
	public boolean isWalkableThru() {
		return false;
	}

	@Override
	public char toChar() {
		switch (state) {
		case Sleeping:
			return 'Z';
		case Laying:
			return '&';
		default:
			return 'D';
		}
	}

	@Override
	public AbstractStepDecision makeStep(int no) {
		if (state == DrinkerState.Sleeping || 
				state == DrinkerState.Laying)
			return null;
		
		AbstractCellObject target = PathFinding.randomStep(cell).getObject();
		if (target != null) {
			if (target.getClass().equals(PillarCellObject.class)) {
				return new DrinkerSleepStepDecision(target.getX(), target.getY(), cell);
			}
			
			if (target.getClass().equals(DrinkerCellObject.class)) {
				DrinkerCellObject other = (DrinkerCellObject)target;
				if (other.getState() == DrinkerState.Sleeping)
					return new DrinkerSleepStepDecision(target.getX(), target.getY(), cell);
			}
			
			IStuffObject stuff = target.getCell().getStuff();
			if (stuff != null && stuff.getClass().equals(BottleStuffObject.class)) {
				return new DrinkerLaysStepDecision(target.getX(), target.getY(), cell);
			}
			
			int dr = Math.abs(rand.nextInt() % 30);
			if (dr == 15 && hasBottle) {
				hasBottle = false;
				return new DrinkerDropsBottleStepDecision(target.getX(), target.getY(), cell);
			}
			
			return new MoveStepDecision(target.getX(), target.getY(), cell);
		}
		
		return null;
	}
	
	public void setState(DrinkerState newState) {
		state = newState;		
	}
	
	public DrinkerState getState() {
		return state;
	}
	
	private boolean hasBottle = true;
	private static Random rand = new Random();
	private DrinkerState state = DrinkerState.Walking;
}
