package com.g2forge.reassert.core.model.report;

import java.util.List;
import java.util.Map;

import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.contract.IContractApplied;

public interface IOrigins {
	public Map<Artifact<?>, List<IContractApplied>> getOrigins();
}
