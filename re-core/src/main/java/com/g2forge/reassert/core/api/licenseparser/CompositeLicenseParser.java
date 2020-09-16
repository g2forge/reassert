package com.g2forge.reassert.core.api.licenseparser;

import java.util.Collection;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.UnknownLicense;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class CompositeLicenseParser implements ILicenseParser {
	protected final Collection<ILicenseParser> parsers;

	public CompositeLicenseParser(ILicenseParser... parsers) {
		this(HCollection.asList(parsers));
	}

	@Override
	public ILicenseApplied parse(String text) {
		final Set<ILicenseApplied> licenses = getParsers().stream().map(parser -> parser.parse(text)).filter(Objects::nonNull).filter(license -> !(license instanceof UnknownLicense)).collect(Collectors.toSet());
		if (licenses.size() != 1) return new UnknownLicense(text);
		return HCollection.getOne(licenses);
	}
}
