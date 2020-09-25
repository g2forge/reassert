package com.g2forge.reassert.contract;

import com.g2forge.reassert.core.model.contract.IContractApplied;
import com.g2forge.reassert.core.model.report.IFindingConsumer;

public interface IContractComparisonAnalyzer {
	public void analyze(IContractApplied a, IContractApplied b, IFindingConsumer consumer);
}
