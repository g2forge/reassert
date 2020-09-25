package com.g2forge.reassert.contract.model.name;

import com.g2forge.reassert.contract.model.IContractComparisonScheme;
import com.g2forge.reassert.core.model.contract.terms.ITerm;

public interface IContractComparisonName {
	public IContractComparisonScheme<?, ?> getScheme();

	public ITerm getTerm();
}
