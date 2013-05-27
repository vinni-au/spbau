package ru.spbau.storozhev.drunksim.objects;

import java.util.ArrayList;
import java.util.List;

import ru.spbau.storozhev.drunksim.core.*;
import ru.spbau.storozhev.drunksim.stepdecisions.*;

public class LamppostCellObject extends AbstractCellObject {

	public LamppostCellObject(Cell c) {
		super(c);
		computeVisibleCells();
	}
	
	public LamppostCellObject(Cell c, int radius) {
		this(c);
		visibleRadius = radius;		
	}
	
	@Override
	public boolean isWalkableThru() {
		return false;
	}

	@Override
	public char toChar() {
		return 'L';
	}

	@Override
	public AbstractStepDecision makeStep(int no) {
		return null;
	}
	
	public Cell[] getVisibleCells() {
		return visibleCells;
	}
	
	private void computeVisibleCells() {
		List<Cell> list = new ArrayList<Cell>();

		int x = cell.getX();
		int y = cell.getY();
		for (int i = x - visibleRadius; i < x + visibleRadius; ++i) {
			for (int j = y - visibleRadius; j < y + visibleRadius; ++j) {
				if (Math.sqrt((x-i)*(x-i) + (y-j)*(y-j)) <= visibleRadius) {
					Cell c = cell.getField().getCell(i, j);
					if (c != null) {
						list.add(c);
					}
				}
			}
		}
		
		visibleCells = new Cell[list.size()];
		visibleCells = list.toArray(visibleCells);
	}
	
	private int visibleRadius = 3;
	private Cell[] visibleCells;
}
