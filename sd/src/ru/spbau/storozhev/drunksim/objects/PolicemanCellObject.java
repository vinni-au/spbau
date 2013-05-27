package ru.spbau.storozhev.drunksim.objects;

import java.util.Arrays;
import java.util.LinkedList;

import ru.spbau.storozhev.drunksim.core.Cell;
import ru.spbau.storozhev.drunksim.objects.DrinkerCellObject.DrinkerState;
import ru.spbau.storozhev.drunksim.stepdecisions.AbstractStepDecision;
import ru.spbau.storozhev.drunksim.stepdecisions.MoveStepDecision;
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
			
			return stepTo(currentTarget);
		}

		return null;
	}
	
	private AbstractStepDecision stepTo(Cell target) {
		int w = cell.getField().getWidth();
		int h = cell.getField().getHeight();
		int bfield[][] = new int[w][h];
		for (int i = 0; i < w; ++i) {
			Arrays.fill(bfield[i], Integer.MAX_VALUE);
		}
		
		bfield[target.getX()][target.getY()] = 0;
		LinkedList<Cell> q = new LinkedList<>();
		q.add(target);
		out: while (!q.isEmpty()) {
			Cell current = q.removeFirst();
			int level = bfield[current.getX()][current.getY()];
			for (Cell c : cell.getField().getNeighbours(current.getX(), current.getY())) {
				if (c.getX() == cell.getX() &&
						c.getY() == cell.getY())
					break out;
				if (c.isWalkableThru() && 
						bfield[c.getX()][c.getY()] == Integer.MAX_VALUE) {
					bfield[c.getX()][c.getY()] = level+1;
					q.add(c);
				}
			}
		}	
		
		Cell minCell = null;
		int min = Integer.MAX_VALUE;
		for (Cell c : cell.getField().getNeighbours(cell.getX(), cell.getY())) {
			if (bfield[c.getX()][c.getY()] < min) {
				minCell = c;
				min = bfield[c.getX()][c.getY()];
			}
		}
		
		if (minCell != null) {
			return new MoveStepDecision(minCell.getX(), minCell.getY(), cell);
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
