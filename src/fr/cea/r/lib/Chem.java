package fr.cea.r.lib;

import org.openscience.cdk.interfaces.IAtomContainer;
import org.openscience.cdk.inchi.InChIGeneratorFactory;
import org.openscience.cdk.exception.CDKException;

public class Chem {

	public static String getInchi(IAtomContainer ac) {

		String inchi = null;

		try {
			inchi = InChIGeneratorFactory.getInstance().getInChIGenerator(ac).getInchi();
		} catch (CDKException e) {
		}

		return inchi;
	}
}
