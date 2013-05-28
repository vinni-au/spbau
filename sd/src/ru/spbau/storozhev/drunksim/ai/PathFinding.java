package ru.spbau.storozhev.drunksim.ai;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import ru.spbau.storozhev.drunksim.core.Cell;
import ru.spbau.storozhev.drunksim.objects.AbstractCellObject;
import ru.spbau.storozhev.drunksim.objects.IStuffObject;
import ru.spbau.storozhev.drunksim.stepdecisions.AbstractStepDecision;
import ru.spbau.storozhev.drunksim.stepdecisions.MoveStepDecision;

public class PathFinding {
	
	public static AbstractStepDecision stepTo(Cell start, Cell target) {
		int w = start.getField().getWidth();
		int h = start.getField().getHeight();
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
			for (Cell c : start.getField().getNeighbours(current.getX(), current.getY())) {
				if (c.getX() == start.getX() &&
						c.getY() == start.getY())
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
		for (Cell c : start.getField().getNeighbours(start.getX(), start.getY())) {
			if (bfield[c.getX()][c.getY()] < min) {
				minCell = c;
				min = bfield[c.getX()][c.getY()];
			}
		}
		
		if (minCell != null) {
			return new MoveStepDecision(minCell.getX(), minCell.getY(), start);
		}
		
		return null;
	}
	
	public static Cell randomStep(Cell current) {
		return selectRandomCell(current.getNeighbours());
	}
	
	public static Cell randomSearchStep(Cell current) {
		List<Cell> possibleTargets = new ArrayList<>();
		
		for (Cell c : current.getNeighbours()) {
			if (c.isWalkableThru())
				possibleTargets.add(c);
		}
		
		return selectRandomCell(possibleTargets);
	}
	
	public static Cell findCellWithObject(Cell current, Class<? extends AbstractCellObject> objClass) {
		for (Cell c : current.getNeighbours()) {
			AbstractCellObject obj = c.getObject();			
			if (obj != null && obj.getClass().equals(objClass)) {
				return c;
			}
		}
		
		return null;
	}
	
	public static Cell findCellWithStuff(Cell current, Class<? extends IStuffObject> stuffClass) {
		for (Cell c : current.getNeighbours()) {
			IStuffObject stuff = c.getStuff();
			if (stuff != null && stuff.getClass().equals(stuffClass)) {
				return c;
			}
		}
		
		return null;
	}
	
	private static Cell selectRandomCell(List<Cell> cells) {
		if (cells.isEmpty())
			return null;
		
		int r = Math.abs(rand.nextInt()) % cells.size();		
		
		return cells.get(r);
	}
	
	private static Random rand = new Random();
}
