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
		case Walking:
			return 'D';
		case Sleeping:
			return 'Z';
		case Laying:
			return '&';
		default:
			return 'D';
		}
	}

	@Override
	public AbstractStepDecision makeStep() {
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
			if (target.getClass().getName().endsWith(".PillarCellObject"))
				return new DrinkerSleepStepDecision(x, y, cell);
			else
				return new MoveStepDecision(x, y, cell);
		}
		
		return null;
	}
	
	public void setState(DrinkerState newState) {
		state = newState;		
	}
	
	private static Random rand = new Random(System.currentTimeMillis());
	private DrinkerState state;
}
