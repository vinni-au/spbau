package ru.spbau.storozhev.drunksim.stepdecisions;

import ru.spbau.storozhev.drunksim.core.Cell;
import ru.spbau.storozhev.drunksim.core.Field;
import ru.spbau.storozhev.drunksim.objects.EmptyCellObject;

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
				field.setCellObject(cell.getObject());
				field.setCellObject(newObj);
			}
		}
	}

}
