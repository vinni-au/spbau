package ru.spbau.storozhev.drunksim.objects;

import java.util.Random;

import ru.spbau.storozhev.drunksim.core.*;
import ru.spbau.storozhev.drunksim.stepdecisions.*;

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
		
		
		int r = Math.abs(rand.nextInt() % 4);
		int x = cell.getX();
		int y = cell.getY();
		
		if (r == 0)
			y++;
		if (r == 1)
			x++;
		if (r == 2)
			y--;
		if (r == 3)
			x--;
		
		AbstractCellObject target = cell.getField().getCellObject(x, y);
		if (target != null) {
			if (target.getClass().equals(PillarCellObject.class))
				return new DrinkerSleepStepDecision(x, y, cell);
			
			if (target.getClass().equals(DrinkerCellObject.class)) {
				DrinkerCellObject other = (DrinkerCellObject)target;
				if (other.getState() == DrinkerState.Sleeping)
					return new DrinkerSleepStepDecision(x, y, cell);
			}
			
			return new MoveStepDecision(x, y, cell);
		}
		
		return null;
	}
	
	public void setState(DrinkerState newState) {
		state = newState;		
	}
	
	public DrinkerState getState() {
		return state;
	}
	
	private static Random rand = new Random();
	private DrinkerState state = DrinkerState.Walking;
}
