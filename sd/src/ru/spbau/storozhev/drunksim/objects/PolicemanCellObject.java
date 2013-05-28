package ru.spbau.storozhev.drunksim.objects;

import ru.spbau.storozhev.drunksim.ai.PathFinding;
import ru.spbau.storozhev.drunksim.core.Cell;
import ru.spbau.storozhev.drunksim.objects.DrinkerCellObject.DrinkerState;
import ru.spbau.storozhev.drunksim.stepdecisions.AbstractStepDecision;
import ru.spbau.storozhev.drunksim.stepdecisions.PolicemanReturnsStepDecision;
import ru.spbau.storozhev.drunksim.stepdecisions.PolicemanTakesDrinkerStepDecision;

public class PolicemanCellObject extends AbstractCellObject {

	public PolicemanCellObject(Cell c, 
			LamppostCellObject[] lampposts, PoliceStationCellObject station) {
		super(c);
		this.lampposts = lampposts; 
		policeStation = station;
	}

	@Override
	public boolean isWalkableThru() {
		return false;
	}

	@Override
	public char toChar() {
		if (drinker != null)
			return 'P';
		
		return 'p';
	}

	@Override
	public AbstractStepDecision makeStep(int no) {
		policeStation.setPolicemanIsOut(true);
		
		if (currentTarget == null) {
			currentTarget = findTarget();
		}
		
		if (currentTarget != null) {
			for (Cell c : cell.getField().getNeighbours(cell.getX(), cell.getY())) {
				if (c.equals(currentTarget)) {
					currentTarget = null;
					if (drinker == null) { 
						return new PolicemanTakesDrinkerStepDecision(c.getX(), c.getY(), cell);
					} else {
						return new PolicemanReturnsStepDecision(c.getX(), c.getY(), cell);
					}
				}
			}
			
			return PathFinding.stepTo(cell, currentTarget);
		}

		return null;
	}
	
	private Cell findTarget() {
		if (drinker != null) {
			return policeStation.getCell();
		} else {
		
			for (LamppostCellObject lamppost : lampposts) {
				for (Cell cell : lamppost.getVisibleCells()) {
					if (cell.getObject().getClass().equals(DrinkerCellObject.class)) {
						DrinkerCellObject drinker = (DrinkerCellObject)cell.getObject();
						if (drinker.getState() == DrinkerState.Laying) {
							return drinker.getCell();
						}
					}
				}
			}
		
		}
		
		return null;		
	}
	
	public void setDrinker(DrinkerCellObject d) {
		drinker = d;
	}
	
	public PoliceStationCellObject getPoliceStation() {
		return policeStation;
	}
	
	private Cell currentTarget;
	private DrinkerCellObject drinker;
	private LamppostCellObject[] lampposts;
	private PoliceStationCellObject policeStation;
}
