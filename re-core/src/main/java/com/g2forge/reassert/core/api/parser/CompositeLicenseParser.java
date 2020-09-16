package com.g2forge.reassert.core.api.parser;

import java.util.Collection;

import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.UnknownLicense;

public class CompositeLicenseParser extends ACompositeParser<ILicenseApplied> {
	public CompositeLicenseParser(Collection<? extends IParser<? extends ILicenseApplied>> parsers) {
		super(parsers);
	}

	@SafeVarargs
	public CompositeLicenseParser(IParser<? extends ILicenseApplied>... parsers) {
		super(parsers);
	}

	protected ILicenseApplied unknown(String text) {
		return new UnknownLicense(text);
	}
}
