package ru.spbau.storozhev.drunksim.stepdecisions;

import ru.spbau.storozhev.drunksim.core.*;
import ru.spbau.storozhev.drunksim.objects.*;

public class MoveStepDecision extends AbstractStepDecision {
	public MoveStepDecision(int x, int y, Cell c) {
		super(x, y, c);
	}

	@Override
	public void doIt() {
		Field field = cell.getField();
		if (targetX >= 0 && targetX < field.getWidth() && targetY >= 0
				&& targetY < field.getHeight()) {
			if (field.getCellObject(targetX, targetY).isWalkableThru()) {
				EmptyCellObject newObj = new EmptyCellObject(field.getCell(
						cell.getX(), cell.getY()));
				cell.getObject().setCell(field.getCell(targetX, targetY));
				field.setCellObject(cell.getObject(), targetX, targetY);
				field.setCellObject(newObj, newObj.getX(), newObj.getY());
			}
		}
	}

	public int desirableX() {
		return targetX;
	}

	public int desirableY() {
		return targetY;
	}

	@Override
	public boolean isConflictedWith(AbstractStepDecision other) {
		if (other.getClass().getName().endsWith(".MoveStepDecision")) {
			MoveStepDecision o = (MoveStepDecision) other;
			if (targetX == o.targetX && targetY == o.targetY)
				return true;
		}

		return false;
	}

}
