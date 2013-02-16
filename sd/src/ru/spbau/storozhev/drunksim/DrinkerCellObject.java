package ru.spbau.storozhev.drunksim;

import java.util.Random;

public class DrinkerCellObject extends AbstractCellObject {
	public DrinkerCellObject(Cell c) {
		super(c);
	}

	@Override
	public boolean isWalkableThru() {
		return false;
	}

	@Override
	public char toChar() {
		return 'D';
	}

	@Override
	public AbstractStepDecision makeStep() {
		if (sleepSteps > 0) {
			sleepSteps--;
			return null;
		}
		
		int r = rand.nextInt() % 4;
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
	
	void setSleepSteps(int steps) {
		sleepSteps = steps;
	}

	private static Random rand = new Random(System.currentTimeMillis());
	private int sleepSteps = 0;
}
