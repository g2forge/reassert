package com.g2forge.reassert.contract.algorithm.work.model.rule;

import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;

public interface IWorkRules extends IFunction1<ILicenseApplied, RuleWorkType> {
	@Override
	public RuleWorkType apply(ILicenseApplied license);
}
