package com.g2forge.reassert.contract.algorithm.worklicense.model.rule;

import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamily;

public interface IWorkLicenseRulesFactory extends IFunction1<ILicenseFamily, IWorkLicenseRules> {
	@Override
	public IWorkLicenseRules apply(ILicenseFamily combinedLicense);

	public boolean isIncluded(IEdge edge, boolean outgoing);
}
