package ru.spbau.storozhev.drunksim.core;

import java.util.ArrayList;
import java.util.List;

import ru.spbau.storozhev.drunksim.objects.*;
import ru.spbau.storozhev.drunksim.stepdecisions.*;

public class DrunkSimApp {
	public DrunkSimApp(int w, int h) {
		field = new Field(w+1, h+1);
		for (int i = 0; i < w+1; ++i)
			field.setCellObject(new DummyCellObject(field.getCell(0, i)));
		for (int i = 0; i < h+1; ++i)
			field.setCellObject(new DummyCellObject(field.getCell(i, w)));
		field.setCellObject(new PillarCellObject(field.getCell(w/2 + 1, h/2 + 1)));
		field.setCellObject(new PubCellObject(field.getCell(0, 9)));
		field.setCellObject(new LamppostCellObject(field.getCell(3, 10)));
		field.setCellObject(new PoliceStationCellObject(field.getCell(3, h)));
	}
	
	public void run() {
		run(200);		
	}
	
	public void run(int steps) {
		for (int s = 0; s < steps; ++s) {
			System.out.println("Step " + s);
			step(s);
			
			field.print();
		}
		
		System.out.println();
	}
	
	public void run(int steps, int traced[]) {
		int s = 0;
		for (int i = 0; i < traced.length; ++i) {
			while (s != traced[i]) {
				step(s);
				if (++s >= steps) {
					field.print(s);
					return;
				}
			}
			
			field.print(s);
		}
		
		while (++s < steps) 
			;
		
		field.print(s);
		
		System.out.println();
	}
	
	private void step(int no) {
		List<AbstractStepDecision> decs = new ArrayList<AbstractStepDecision>();
		for (int i = 0; i < field.getWidth(); ++i) {
			for (int j = 0; j < field.getHeight(); ++j) {
				AbstractStepDecision d = field.getCellObject(i, j).makeStep(no); 
				if (d != null) {
					decs.add(d);
				}
			}
		}
		
		for (AbstractStepDecision sd : decs) {
			sd.doIt();
		}				
	}
	
	private Field field;
}
