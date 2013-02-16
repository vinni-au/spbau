import ru.spbau.storozhev.drunksim.DrunkSimApp;


public class Main {
	public static void main(String[] args) {
		System.out.println("Starting DrunkSimApp...");
		int w = 15;
		int h = 15;
		int steps = 200;
		if (args.length == 3) {
			System.out.println("Using first two parameters as field sizes and third parameter as step count");
			w = Integer.parseInt(args[0]);
			h = Integer.parseInt(args[1]);
			steps = Integer.parseInt(args[2]);
		}
		DrunkSimApp app = new DrunkSimApp(w, h);
		try {
			app.run(steps);
		} catch (Exception e) {
			e.printStackTrace();
		}
		System.out.println("Done.");
	}
}
