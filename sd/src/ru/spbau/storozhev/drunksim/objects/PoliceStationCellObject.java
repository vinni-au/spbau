package ru.spbau.storozhev.drunksim.objects;

import ru.spbau.storozhev.drunksim.core.Cell;
import ru.spbau.storozhev.drunksim.objects.DrinkerCellObject.DrinkerState;
import ru.spbau.storozhev.drunksim.stepdecisions.AbstractStepDecision;
import ru.spbau.storozhev.drunksim.stepdecisions.GoOutStepDecision;

public class PoliceStationCellObject extends AbstractCellObject {

	public PoliceStationCellObject(Cell c, LamppostCellObject[] lposts) {
		super(c);
		lampposts = lposts;
	}

	@Override
	public boolean isWalkableThru() {
		return false;
	}

	@Override
	public char toChar() {
		return 'S';
	}

	@Override
	public AbstractStepDecision makeStep(int no) {
		if (policemanIsOut)
			return null; 
		
		for (LamppostCellObject lamppost : lampposts) {
			for (Cell cell : lamppost.getVisibleCells()) {
				if (cell.getObject().getClass().equals(DrinkerCellObject.class)) {
					DrinkerCellObject drinker = (DrinkerCellObject)cell.getObject();
					if (drinker.getState() == DrinkerState.Laying) {
						return new GoOutStepDecision(getX(), getY() - 1, getCell(), 
								new PolicemanCellObject(getCell(), lampposts, this));
					}
				}
			}
		}
		
		return null;
	}
	
	public void setPolicemanIsOut(boolean value) {
		policemanIsOut = value;
	}
	
	private boolean policemanIsOut = false;
	private LamppostCellObject[] lampposts;

}
