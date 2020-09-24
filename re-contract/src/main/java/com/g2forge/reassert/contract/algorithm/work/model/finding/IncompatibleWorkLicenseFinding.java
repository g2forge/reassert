package com.g2forge.reassert.contract.algorithm.work.model.finding;

import java.util.Set;

import org.slf4j.event.Level;

import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class IncompatibleWorkLicenseFinding implements IWorkFinding {
	@Singular("unknown")
	protected final Set<ILicenseTerm> unknown;

	@Singular("mismatched")
	protected final Set<ILicenseTerm> mismatched;

	@Override
	public Level getLevel() {
		return (getUnknown().isEmpty() && getMismatched().isEmpty()) ? Level.INFO : Level.ERROR;
	}
}