package ru.spbau.storozhev.drunksim.core;

import java.util.ArrayList;
import java.util.List;

import ru.spbau.storozhev.drunksim.objects.*;
import ru.spbau.storozhev.drunksim.stepdecisions.*;

public class DrunkSimApp {
	public DrunkSimApp(int w, int h) {
		field = new Field(w, h);
		field.setCellObject(new DrinkerCellObject(field.getCell(0, 0)), 0, 0);
		field.setCellObject(new PillarCellObject(field.getCell(w/2, h/2)), w/2, h/2);
	}
	
	public void run() {
		run(200);		
	}
	
	public void run(int steps) {
		for (int s = 0; s < steps; ++s) {
			System.out.println("Step " + s);
			step();
			
			field.print();
		}
		
		System.out.println();
	}
	
	private void step() {
		List<AbstractStepDecision> decs = new ArrayList<AbstractStepDecision>();
		for (int i = 0; i < field.getWidth(); ++i) {
			for (int j = 0; j < field.getHeight(); ++j) {
				AbstractStepDecision d = field.getCellObject(i, j).makeStep(); 
				if (d != null) {
					decs.add(d);
				}
			}
		}
		for (int i = 0; i < decs.size(); ++i) {
			AbstractStepDecision sd = decs.get(i);
			sd.doIt();
		}		
		
	}
	
	private Field field;
//	private boolean isRunning = true;
}
