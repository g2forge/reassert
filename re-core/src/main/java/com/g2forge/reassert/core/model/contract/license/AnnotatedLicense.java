package com.g2forge.reassert.core.model.contract.license;

import java.util.List;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.core.helpers.HCollector;
import com.g2forge.reassert.core.model.contract.terms.ITerms;

import lombok.Builder;
import lombok.Data;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
public class AnnotatedLicense implements ILicense {
	protected final ILicenseFamily license;

	/** The parsed text which resulted in this license if it's interesting (the parser may leave this off if the license exactly matches one of them). */
	protected final String text;

	@Singular
	protected final List<ILicenseException> exceptions;

	public AnnotatedLicense(ILicenseFamily license, String text, List<ILicenseException> exceptions) {
		this.license = license;
		this.text = text;
		this.exceptions = exceptions;

		if (exceptions != null) {
			final List<ILicenseException> inapplicable = exceptions.stream().filter(exception -> !exception.isApplicable(license)).collect(Collectors.toList());
			throw new IllegalArgumentException(String.format("Inapplicable exception(s): %1$s", inapplicable.stream().map(exception -> exception.getShortID()).collect(HCollector.joining(", ", "&"))));
		}
	}

	@Override
	public ITerms<ILicenseTerm> getTerms() {
		return getLicense().getTerms();
	}
}
