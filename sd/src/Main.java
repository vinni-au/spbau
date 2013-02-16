import ru.spbau.storozhev.drunksim.DrunkSimApp;


public class Main {
	public static void main(String[] args) {
		System.out.println("Starting DrunkSimApp...");
		DrunkSimApp app = new DrunkSimApp();
		try {
			app.run(15, 15);
		} catch (Exception e) {
			e.printStackTrace();
		}
		System.out.println("Done.");
	}
}
