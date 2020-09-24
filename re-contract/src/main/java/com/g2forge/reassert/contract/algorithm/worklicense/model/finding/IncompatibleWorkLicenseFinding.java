package com.g2forge.reassert.contract.algorithm.worklicense.model.finding;

import java.util.Set;

import org.slf4j.event.Level;

import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.report.ITerminalFinding;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class IncompatibleWorkLicenseFinding implements ITerminalFinding {
	@Singular("unknown")
	protected final Set<ILicenseTerm> unknown;

	@Singular("mismatched")
	protected final Set<ILicenseTerm> mismatched;

	@Override
	public Level getLevel() {
		return (getUnknown().isEmpty() && getMismatched().isEmpty()) ? Level.INFO : Level.ERROR;
	}
}