package com.g2forge.reassert.list.convert;

import com.fasterxml.jackson.databind.module.SimpleAbstractTypeResolver;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.g2forge.reassert.core.model.contract.terms.ITerms;
import com.g2forge.reassert.core.model.contract.terms.Terms;

public class TermsModule extends SimpleModule {
	private static final long serialVersionUID = -3917063528880853168L;

	@Override
	public void setupModule(SetupContext context) {
		final SimpleAbstractTypeResolver resolver = new SimpleAbstractTypeResolver();
		resolver.addMapping(ITerms.class, Terms.class);
		setAbstractTypes(resolver);

		super.setupModule(context);
	}
}